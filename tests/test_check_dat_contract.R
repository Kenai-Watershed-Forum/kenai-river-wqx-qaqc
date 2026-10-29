# Tests for functions/check_dat_contract.R. All inputs are synthetic; nothing depends on
# a particular year's data or on row counts.
#
# Run: testthat::test_file(here::here("tests/test_check_dat_contract.R"))

library(testthat)
library(dplyr)
source(here::here("functions", "check_dat_contract.R"))

ref <- here::here("reference")
match_xlsx <- file.path(ref, "wqx_templates/wqx_template_matching_table.xlsx")
site_id <- as.character(readxl::read_excel(match_xlsx, sheet = "site_coordinates")$monitoring_location_id[1])
analyte <- utils::read.csv(file.path(ref, "wqx_templates/analytes_list_manual_edit.csv"))$analyte[1]
method  <- readxl::read_excel(match_xlsx, sheet = "result_sample_fraction")$analytical_method[1]

cfg <- list(year = 2031, sampling = list(spring_date = "2031-05-01", summer_date = "2031-07-20"))

good <- tibble::tibble(
  sample = "S1", lab_sample = "L1", lab_name = "LabA", analyte = analyte, result = 1.5,
  units = "mg/L", resultflag = "=", loq = 1, lod = 0.5,
  collect_date = as.Date("2031-05-01"), collect_time = hms::as_hms("10:00:00"),
  rec_date = as.Date("2031-05-02"), rec_time = hms::as_hms("09:00:00"),
  run_date = as.Date("2031-05-03"), run_time = hms::as_hms("08:00:00"),
  analytical_method = method, epa_analysis_id = method, context_code = "USEPA",
  monitoring_location_id = site_id, note = NA_character_, sample_type = "PS",
  sample_condition = NA_character_, dissolved = NA_character_,
  result_sample_fraction = "Total"
)

issues_for <- function(d, ...) check_dat_contract(d, cfg, stop_on_error = FALSE, ...) |>
  suppressMessages()
has_issue <- function(d, pattern, ...) any(grepl(pattern, issues_for(d, ...)$check, fixed = TRUE))

test_that("a valid frame passes with no errors", {
  expect_equal(nrow(filter(issues_for(good), severity == "error")), 0)
  expect_no_error(suppressMessages(check_dat_contract(good, cfg)))
})

test_that("structure is enforced", {
  expect_true(has_issue(select(good, -units), "columns missing"))
  expect_true(has_issue(mutate(good, matrix = "Water"), "unexpected extra columns"))
  expect_true(has_issue(mutate(good, result = "1.5"), "wrong column type"))
  expect_true(has_issue(mutate(good, collect_date = "2031-05-01"), "wrong column type"))
})

test_that("required values and flags are enforced", {
  expect_true(has_issue(mutate(good, analyte = NA_character_), "NA in required column: analyte"))
  expect_true(has_issue(mutate(good, resultflag = "X"), "resultflag not one of"))
  expect_true(has_issue(mutate(good, result = NA_real_), "result is NA but"))
  expect_false(has_issue(mutate(good, result = NA_real_, resultflag = "U"), "result is NA but"))
})

test_that("units must be WQX codes and must not mix capitalization", {
  expect_true(has_issue(mutate(good, units = "furlongs"), "unit not in the WQX UoM list"))
  expect_false(has_issue(mutate(good, units = "mg/l"), "unit not in the WQX UoM list"))
  mixed <- bind_rows(good, mutate(good, units = "mg/l", sample = "S2"))
  expect_true(has_issue(mixed, "different capitalization"))
})

test_that("location IDs must be bare digits and known", {
  expect_true(has_issue(mutate(good, monitoring_location_id = paste0("KENAI_WQX-", site_id)),
                        "not bare digits"))
  expect_true(has_issue(mutate(good, monitoring_location_id = "99999999"), "not in site_coordinates"))
})

test_that("lab QC rows and surrogates are rejected", {
  expect_true(has_issue(mutate(good, sample_type = "MB"), "lab QC sample_type in dat"))
  expect_true(has_issue(mutate(good, analyte = "Toluene-d8 (surr)"), "surrogate analyte in dat"))
})

test_that("sample_condition values are restricted", {
  expect_false(has_issue(mutate(good, sample_condition = "Field Blank"), "sample_condition not"))
  expect_true(has_issue(mutate(good, sample_condition = "Blank"), "sample_condition not"))
})

test_that("lookup and duplicate-key rules fire", {
  expect_true(has_issue(mutate(good, analyte = "Unobtainium"), "analyte not in analytes_list"))
  expect_true(has_issue(bind_rows(good, good), "duplicate result key"))
  unmapped <- mutate(good, analytical_method = "NOPE")
  expect_equal(filter(issues_for(unmapped), grepl("matching table", check))$severity[1], "warning")
})

test_that("config consistency is enforced", {
  expect_true(has_issue(mutate(good, collect_date = as.Date("2031-06-15")), "not config sampling"))
  expect_true(has_issue(mutate(good, collect_date = as.Date("2030-05-01")), "year differs"))
  expect_true(has_issue(mutate(good, run_date = as.Date("2031-04-30")), "run_date is before"))
  expect_true(has_issue(mutate(good, rec_date = as.Date("2031-04-30")), "rec_date is before"))
  expect_true(has_issue(mutate(good, rec_date = as.Date("2031-05-09")), "rec_date is after run_date"))
})

test_that("lab_qc minimal contract", {
  qc <- tibble::tibble(sample_type = "MB", qc_group = "blank", analyte = analyte,
                       analytical_method = method, result = 0, resultflag = "U", units = "mg/L",
                       collect_date = as.Date("2031-05-01"), run_date = as.Date("2031-05-03"),
                       lab_name = "LabA")
  expect_equal(nrow(filter(issues_for(good, lab_qc = qc), severity == "error")), 0)
  expect_true(has_issue(good, "qc_group", lab_qc = mutate(qc, qc_group = "bogus")))
  expect_true(has_issue(good, "lab_qc missing", lab_qc = select(qc, -qc_group)))
})
