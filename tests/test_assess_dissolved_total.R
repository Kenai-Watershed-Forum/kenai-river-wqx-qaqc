# Tests for functions/assess_dissolved_total.R. Synthetic rows only.
#
# Run: testthat::test_file(here::here("tests/test_assess_dissolved_total.R"))

library(testthat)
source(here::here("functions", "assess_rpd.R"))
source(here::here("functions", "assess_dissolved_total.R"))

row <- function(site = "1", frac, value, unit = "ug/L", cond = NA_character_, ch = "Zinc",
                type = "Field Msr/Obs", dat_row = 1L) {
  data.frame(monitoring_location_id = site, activity_start_date = "2025-04-30",
             activity_type = type, characteristic_name = ch, result_sample_fraction = frac,
             result_value = value, result_unit = unit, result_detection_condition = cond,
             dat_row = dat_row, stringsAsFactors = FALSE)
}
pair <- function(d, t, tfrac = "Unfiltered", ...) {
  rbind(row(frac = "Dissolved", value = d, dat_row = 1L, ...),
        row(frac = tfrac, value = t, dat_row = 2L, ...))
}

test_that("dissolved <= total is OK; Total and Unfiltered both count as total", {
  expect_equal(assess_dissolved_total(pair(5, 10))$dt_status, "OK")
  expect_equal(assess_dissolved_total(pair(5, 10, "Total"))$dt_status, "OK")
})

test_that("dissolved slightly above total is within tolerance; far above exceeds it", {
  # RPD 10% for 10.5 vs 9.5 ... limit is 20 for zinc
  expect_equal(assess_dissolved_total(pair(10.5, 9.5))$dt_status,
               "Dissolved > total (within tolerance)")
  r <- assess_dissolved_total(pair(15, 10))
  expect_equal(r$dt_status, "Dissolved > total (exceeds tolerance)")
  expect_equal(r$rpd_pct, 40)
  expect_equal(c(r$dissolved_dat_row, r$total_dat_row), 1:2)
})

test_that("the tolerance follows rpd_threshold by characteristic", {
  expect_equal(assess_dissolved_total(pair(12, 10, ch = "Nitrate"))$threshold, 25)
  expect_equal(assess_dissolved_total(pair(12, 10, ch = "Nitrate"))$dt_status,
               "Dissolved > total (within tolerance)")
})

test_that("non-detects and below-quantification results are Not comparable", {
  d <- rbind(row(frac = "Dissolved", value = 0, cond = "Not Detected", dat_row = 1L),
             row(frac = "Unfiltered", value = 3, dat_row = 2L))
  expect_equal(assess_dissolved_total(d)$dt_status, "Not comparable")
  d2 <- rbind(row(frac = "Dissolved", value = 4, dat_row = 1L),
              row(frac = "Unfiltered", value = 2, cond = "Present Below Quantification Limit",
                  dat_row = 2L))
  expect_equal(assess_dissolved_total(d2)$dt_status, "Not comparable")
})

test_that("units are converted to ug/L; unknown units stop", {
  d <- rbind(row(frac = "Dissolved", value = 0.005, unit = "mg/L", dat_row = 1L),
             row(frac = "Unfiltered", value = 10, unit = "ug/L", dat_row = 2L))
  r <- assess_dissolved_total(d)
  expect_equal(r$dissolved_result, 5)
  expect_equal(r$dt_status, "OK")
  d$result_unit[1] <- "ppm"
  expect_error(assess_dissolved_total(d), "unrecognized unit")
})

test_that("only primary samples with both fractions are paired", {
  one_sided <- row(frac = "Unfiltered", value = 5)
  expect_equal(nrow(assess_dissolved_total(one_sided)), 0)
  qc <- pair(15, 10, type = "Quality Control Sample-Field Blank")
  expect_equal(nrow(assess_dissolved_total(qc)), 0)
  other_site <- rbind(row(site = "1", frac = "Dissolved", value = 15, dat_row = 1L),
                      row(site = "2", frac = "Unfiltered", value = 10, dat_row = 2L))
  expect_equal(nrow(assess_dissolved_total(other_site)), 0)
})

test_that("two primary results for one fraction stop; missing columns stop", {
  d <- rbind(pair(5, 10), row(frac = "Dissolved", value = 6, dat_row = 3L))
  expect_error(assess_dissolved_total(d), "more than one")
  expect_error(assess_dissolved_total(pair(5, 10)[, -1]), "missing column")
})

mf <- function(flag, frac = NA_character_, ch = "Zinc") {
  data.frame(activity_start_date = as.Date("2025-04-30"), characteristic_name = ch,
             monitoring_location_id = "1", result_sample_fraction = frac, flag = flag,
             notes = "reviewed", stringsAsFactors = FALSE)
}

test_that("an undecided exceedance stops with the pair listed", {
  p <- assess_dissolved_total(pair(15, 10))
  expect_error(check_dissolved_total_decisions(p, NULL), "no decision is recorded")
  expect_error(check_dissolved_total_decisions(p, NULL), "Zinc")
  expect_error(check_dissolved_total_decisions(p, mf("")), "no decision is recorded")
})

test_that("a Y or N decision satisfies the check; within tolerance needs none", {
  p <- assess_dissolved_total(pair(15, 10))
  expect_silent(check_dissolved_total_decisions(p, mf("Y")))
  expect_silent(check_dissolved_total_decisions(p, mf("N", "Dissolved")))
  expect_silent(check_dissolved_total_decisions(assess_dissolved_total(pair(10.5, 9.5)), NULL))
  expect_equal(nrow(check_dissolved_total_decisions(p, NULL, stop_on_missing = FALSE)), 1)
})
