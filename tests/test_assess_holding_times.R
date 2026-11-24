# Tests for functions/assess_holding_times.R. Synthetic rows only.
#
# Run: testthat::test_file(here::here("tests/test_assess_holding_times.R"))

library(testthat)
source(here::here("functions", "assess_holding_times.R"))

limits <- data.frame(
  result_analytical_method_id = c("9222D", "2540-D", "", NA),
  max_holding_time_text = c("6 hours", "7 days", "", ""),
  max_holding_time_hours = c(6, 168, NA, NA),
  stringsAsFactors = FALSE
)
in_situ <- data.frame(analytical_method = "150.1", stringsAsFactors = FALSE)

tm <- function(x) hms::as_hms(ifelse(is.na(x), NA_character_, paste0(x, ":00")))

mk <- function(method, cd = "2025-04-30", ct = "09:00", rd = "2025-04-30", rt = "13:00",
               rec_d = NA, rec_t = NA) {
  n <- length(method)
  data.frame(
    dat_row = seq_len(n), sample = "S", analyte = "A", lab_name = "L",
    epa_analysis_id = method,
    collect_date = as.Date(cd), collect_time = tm(ct),
    run_date = as.Date(rd), run_time = tm(rt),
    rec_date = as.Date(rec_d), rec_time = tm(rec_t),
    stringsAsFactors = FALSE
  )
}

test_that("pass and fail use collection to analysis start", {
  d <- rbind(mk("9222D", rt = "14:30"), mk("9222D", rt = "15:30"))
  d$dat_row <- 1:2
  out <- assess_holding_times(d, limits, in_situ)
  expect_equal(out$hold_status, c("Pass", "Fail"))
  expect_equal(out$elapsed_hours, c(5.5, 6.5))
  expect_match(out$hold_note[2], "Hold time exceeded")
})

test_that("exactly at the limit passes", {
  out <- assess_holding_times(mk("9222D", rt = "15:00"), limits, in_situ)
  expect_equal(out$hold_status, "Pass")
})

test_that("in-situ methods are Not applicable", {
  out <- assess_holding_times(mk("150.1"), limits, in_situ)
  expect_equal(out$hold_status, "Not applicable")
  expect_equal(out$receipt_check, "Not applicable")
})

test_that("a method with no limit is Not evaluable, never Pass", {
  out <- assess_holding_times(mk("XYZ"), limits, in_situ)
  expect_equal(out$hold_status, "Not evaluable")
  expect_match(out$hold_note, "No holding time limit")
})

test_that("missing run time is bracketed", {
  # 7 day limit: run date 7 days later, any time of day, can exceed it (collected 09:00)
  d <- mk("2540-D", rd = "2025-05-07", rt = NA)
  expect_equal(assess_holding_times(d, limits, in_situ)$hold_status, "Not evaluable")
  # Within 6 days regardless of time: Pass
  d <- mk("2540-D", rd = "2025-05-05", rt = NA)
  expect_equal(assess_holding_times(d, limits, in_situ)$hold_status, "Pass")
  # Beyond 8 days regardless of time: Fail
  d <- mk("2540-D", rd = "2025-05-10", rt = NA)
  expect_equal(assess_holding_times(d, limits, in_situ)$hold_status, "Fail")
})

test_that("missing dates and backwards dates are Not evaluable", {
  d <- mk("2540-D"); d$run_date <- as.Date(NA)
  expect_match(assess_holding_times(d, limits, in_situ)$hold_note, "date missing")
  d <- mk("2540-D", rd = "2025-04-29")
  expect_match(assess_holding_times(d, limits, in_situ)$hold_note, "precedes")
})

test_that("receipt cross-check does not change the hold status", {
  ok <- assess_holding_times(mk("2540-D", rec_d = "2025-04-30", rec_t = "11:00"), limits, in_situ)
  bad <- assess_holding_times(mk("2540-D", rec_d = "2025-05-20", rec_t = "15:00"), limits, in_situ)
  none <- assess_holding_times(mk("2540-D"), limits, in_situ)
  expect_equal(c(ok$receipt_check, bad$receipt_check, none$receipt_check),
               c("OK", "Out of order", "Not recorded"))
  expect_equal(c(ok$hold_status, bad$hold_status, none$hold_status), rep("Pass", 3))
})

test_that("missing columns and duplicate limits stop", {
  expect_error(assess_holding_times(mk("2540-D")[, -1], limits, in_situ), "missing column")
  expect_error(assess_holding_times(mk("2540-D"), rbind(limits, limits[1, ]), in_situ),
               "duplicate")
})
