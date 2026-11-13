# Tests for functions/combine_dat.R using stub ingest functions (no raw lab files needed).
#
# Run: testthat::test_file(here::here("tests/test_combine_dat.R"))

library(testthat)
library(dplyr)
source(here::here("functions", "check_dat_contract.R"))
source(here::here("functions", "combine_dat.R"))

ref <- here::here("reference")
match_xlsx <- file.path(ref, "wqx_templates/wqx_template_matching_table.xlsx")
site_id <- as.character(readxl::read_excel(match_xlsx, sheet = "site_coordinates")$monitoring_location_id[1])
analyte <- utils::read.csv(file.path(ref, "wqx_templates/analytes_list_manual_edit.csv"))$analyte[1]
method  <- readxl::read_excel(match_xlsx, sheet = "result_sample_fraction")$analytical_method[1]

cfg <- list(
  year = 2031, sampling = list(spring_date = "2031-05-01", summer_date = "2031-07-20"),
  files = list(sgs = list(spring = 1, summer = 1), fc = list(spring = 1, summer = 1),
               tss = list(spring = 1, summer = 1), ysi = list(both = 1)))

make_dat <- function(date, lab, sample) tibble::tibble(
  sample = sample, lab_sample = "L1", lab_name = lab, analyte = analyte, result = 1.5,
  units = "mg/L", resultflag = "=", loq = 1, lod = 0.5,
  collect_date = as.Date(date), collect_time = hms::as_hms("10:00:00"),
  rec_date = as.Date(NA), rec_time = hms::as_hms(NA),
  run_date = as.Date(date), run_time = hms::as_hms("08:00:00"),
  analytical_method = method, epa_analysis_id = paste(method, lab), context_code = "USEPA",
  monitoring_location_id = site_id, note = NA_character_, sample_type = "PS",
  sample_condition = NA_character_, dissolved = NA_character_, result_sample_fraction = "Total")

empty_qc <- tibble::tibble(
  sample_type = "MB", qc_group = "blank", analyte = analyte, analytical_method = method,
  result = 0, resultflag = "U", units = "mg/L", collect_date = as.Date("2031-05-01"),
  run_date = as.Date("2031-05-01"), lab_name = "LabA")

stub <- function(lab) function(season = "both", config, data_dir, ...) {
  date <- if (season == "summer") "2031-07-20" else "2031-05-01"
  list(dat = make_dat(date, lab, paste(lab, season)), lab_qc = empty_qc,
       excluded = tibble::tibble(x = 1))
}
stubs <- list(sgs = stub("SGS"), fc = stub("FC"), tss = stub("TSS"), ysi = stub("YSI"))

test_that("seasons come from config and results are row-bound", {
  res <- suppressMessages(combine_dat(cfg, ingest_fns = stubs, stop_on_error = FALSE))
  # 3 labs x 2 seasons + 1 both-season YSI file
  expect_equal(nrow(res$dat), 7)
  expect_equal(nrow(res$lab_qc), 7)
  expect_true(is.character(res$dat$monitoring_location_id))
  expect_equal(nrow(res$ysi_excluded), 1)
})

test_that("a contract violation in the combined data stops the run", {
  bad <- stubs
  bad$tss <- function(season, config, data_dir, ...) {
    out <- stubs$tss(season, config, data_dir)
    out$dat$units <- "furlongs"
    out
  }
  expect_error(suppressMessages(combine_dat(cfg, ingest_fns = bad)), "contract violations")
  expect_no_error(suppressMessages(combine_dat(cfg, ingest_fns = bad, stop_on_error = FALSE)))
})
