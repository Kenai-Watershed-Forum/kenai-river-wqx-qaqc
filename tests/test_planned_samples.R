library(testthat)
library(dplyr)
source(here::here("functions/planned_samples.R"))

sites <- tibble::tibble(river_mile = c(1.5, 10, 30, 31, 40),
                        monitoring_location_id = c("A", "B", "C", "D", "E"),
                        location_name = NA_character_)
groups <- tibble::tribble(
  ~analyte_group, ~analyte, ~sample_fraction,
  "Dissolved metals", "Lead", "Dissolved",
  "Total metals", "Calcium", "Unfiltered",
  "Total metals", "Zinc", "Unfiltered",
  "Hydrocarbons (BTEX)", "Benzene", NA,
  "TSS", "Total suspended solids", NA)

mk_rules <- function() tibble::tribble(
  ~analyte_group, ~analyte, ~sample_fraction, ~spring, ~summer, ~site_rule, ~rm_min, ~rm_max, ~add_rm, ~exclude_rm,
  "Dissolved metals", "Lead", "Dissolved", "Y", "Y", "range", NA, "30", NA, NA,
  "Total metals", "Calcium", "Unfiltered", "Y", "Y", "all", NA, NA, NA, "40",
  "Total metals", "Zinc", "Unfiltered", "N", "Y", "all", NA, NA, NA, NA,
  "Hydrocarbons (BTEX)", "Benzene", NA, "N", "Y", "list", NA, NA, "1.5;40", NA,
  "TSS", "Total suspended solids", NA, "Y", "Y", "all", NA, NA, NA, NA)

mk_qc <- function() tibble::tribble(
  ~season, ~sample_type, ~river_mile, ~analyte_group, ~confirm,
  "spring", "Field Duplicate", "10", "All analytes", "Y",
  "summer", "Field Blank", "10", "Dissolved metals; Total metals", "Y",
  "summer", "Trip Blank", "1.5;40", "Hydrocarbons (BTEX)", "Y")

mk_plan <- function(rules = mk_rules(), qc = mk_qc()) list(rules = rules, qc = qc, sites = sites, groups = groups)

test_that("a complete plan passes the check", {
  expect_silent(check_planned_samples(mk_plan()))
})

test_that("range, all, list rules and exclusions expand to the right sites", {
  e <- expand_planned_samples(mk_plan()) |> filter(sample_type == "Primary")
  expect_setequal(e$river_mile[e$analyte == "Lead" & e$season == "summer"], c(1.5, 10, 30))
  expect_setequal(e$river_mile[e$analyte == "Calcium" & e$season == "spring"], c(1.5, 10, 30, 31))
  expect_setequal(e$river_mile[e$analyte == "Benzene"], c(1.5, 40))
  expect_false("Benzene" %in% e$analyte[e$season == "spring"])
  expect_false("Zinc" %in% e$analyte[e$season == "spring"])
})

test_that("duplicates copy the primary plan at the site; blanks use the group analytes", {
  e <- expand_planned_samples(mk_plan())
  dup <- e |> filter(sample_type == "Field Duplicate")
  expect_setequal(dup$analyte, c("Lead", "Calcium", "Total suspended solids"))
  expect_true(all(dup$season == "spring" & dup$monitoring_location_id == "B"))
  fb <- e |> filter(sample_type == "Field Blank")
  expect_setequal(fb$analyte, c("Lead", "Calcium", "Zinc"))
  tb <- e |> filter(sample_type == "Trip Blank")
  expect_equal(nrow(tb), 2)
  expect_setequal(tb$monitoring_location_id, c("A", "E"))
})

test_that("the gate lists every unanswered cell", {
  r <- mk_rules(); r$spring[3] <- NA
  q <- mk_qc(); q$river_mile[1] <- NA; q$confirm[2] <- NA
  err <- tryCatch(check_planned_samples(mk_plan(r, q)), error = conditionMessage)
  expect_match(err, "analyte_rules row 4 .*spring must be Y or N")
  expect_match(err, "qc_samples row 2 .*river_mile is blank")
  expect_match(err, "qc_samples row 3 .*confirm must be Y")
})

test_that("the gate rejects bad rules and unknown values", {
  r <- mk_rules(); r$site_rule[1] <- "range"; r$rm_max[1] <- NA
  expect_error(check_planned_samples(mk_plan(r)), "range rule needs")
  r <- mk_rules(); r$add_rm[4] <- "99"
  expect_error(check_planned_samples(mk_plan(r)), "not in site_list")
  r <- mk_rules(); r$analyte[5] <- "Mystery"
  expect_error(check_planned_samples(mk_plan(r)), "analyte_groups.csv")
  q <- mk_qc(); q$analyte_group[2] <- "All analytes"
  expect_error(check_planned_samples(mk_plan(qc = q)), "only to Field Duplicate")
  q <- mk_qc(); q$sample_type[1] <- "Primary"
  expect_error(check_planned_samples(mk_plan(qc = q)), "sample_type must be")
  q <- mk_qc(); q$river_mile[1] <- "77"
  expect_error(check_planned_samples(mk_plan(qc = q)), "not in site_list")
})

mk_dat <- function(...) {
  defaults <- list(collect_date = as.Date("2031-05-01"), sample_condition = NA_character_,
                   result_sample_fraction = NA_character_, qc_status = "Accepted")
  args <- list(...)
  tibble::as_tibble(c(args, defaults[setdiff(names(defaults), names(args))]))
}

test_that("comparison reports received, missing and unplanned results", {
  planned <- expand_planned_samples(mk_plan()) |>
    filter(season == "spring", sample_type == "Primary", analyte %in% c("Lead", "Total suspended solids"))
  dat <- bind_rows(
    mk_dat(monitoring_location_id = "A", analyte = "Lead", result_sample_fraction = "Dissolved"),
    mk_dat(monitoring_location_id = "D", analyte = "Lead", result_sample_fraction = "Dissolved"),
    mk_dat(monitoring_location_id = "A", analyte = "Total suspended solids", result_sample_fraction = "Total"),
    mk_dat(monitoring_location_id = "B", analyte = "Total suspended solids", qc_status = "Rejected"))
  cmp <- compare_planned_to_dat(planned, dat, "2031-05-01", "2031-07-20")
  pick <- function(site, an) cmp[cmp$monitoring_location_id == site & cmp$analyte == an, ]
  expect_equal(pick("A", "Lead")$received, 1L)
  expect_equal(pick("D", "Lead")$unplanned, 1L)
  expect_equal(pick("D", "Lead")$planned, 0L)
  expect_equal(pick("B", "Lead")$missing, 1L)
  # Fraction is ignored for analytes the plan does not split by fraction
  expect_equal(pick("A", "Total suspended solids")$received, 1L)
  expect_equal(pick("A", "Total suspended solids")$missing, 0L)
  expect_equal(pick("B", "Total suspended solids")$accepted_planned, 0L)
  expect_equal(sum(cmp$planned), nrow(planned))
})

test_that("QC samples match on sample type and season uses the nearest event date", {
  planned <- tibble::tibble(season = "summer", sample_type = "Field Duplicate", river_mile = 10,
                            monitoring_location_id = "B", analyte = "Lead",
                            sample_fraction = "Dissolved", analyte_group = "Dissolved metals",
                            source = "x")
  dat <- bind_rows(
    mk_dat(monitoring_location_id = "B", analyte = "Lead", result_sample_fraction = "Dissolved",
           sample_condition = "Field Duplicate", collect_date = as.Date("2031-07-19")),
    mk_dat(monitoring_location_id = "B", analyte = "Lead", result_sample_fraction = "Dissolved",
           collect_date = as.Date("2031-07-19")))
  cmp <- compare_planned_to_dat(planned, dat, "2031-05-01", "2031-07-20")
  expect_equal(cmp$received[cmp$sample_type == "Field Duplicate"], 1L)
  expect_equal(cmp$unplanned[cmp$sample_type == "Primary"], 1L)
})

test_that("the workbook passes the gate once its answers are filled in", {
  skip_if_not(file.exists(here::here("year_files/planned_samples.xlsx")))
  plan <- read_planned_samples()
  # Blank the answer cells so the test holds for a blank or a filled workbook
  plan$rules$spring[plan$rules$needs_confirm %in% "Y"] <- NA
  plan$qc$river_mile[1] <- NA
  expect_error(check_planned_samples(plan), "needs attention")
  plan$rules$spring[is.na(plan$rules$spring)] <- "Y"; plan$rules$summer[is.na(plan$rules$summer)] <- "Y"
  plan$qc$river_mile[plan$qc$sample_type != "Trip Blank"] <- "10"
  plan$qc$river_mile[plan$qc$sample_type == "Trip Blank"] <- "1.5;6.5"
  plan$qc$confirm <- "Y"
  expect_silent(check_planned_samples(plan))
  expect_gt(nrow(expand_planned_samples(plan)), 0)
})
