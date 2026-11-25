# Tests for functions/finalize_qc_status.R. Synthetic rows only.
#
# Run: testthat::test_file(here::here("tests/test_finalize_qc_status.R"))

library(testthat)
source(here::here("functions", "finalize_qc_status.R"))

orig <- "Field Msr/Obs"
rep_type <- "Quality Control Field Replicate Msr/Obs"

mk_export <- function() {
  data.frame(
    dat_row = 1:5,
    monitoring_location_id = c("1", "1", "1", "2", "1"),
    activity_start_date = "2025-04-30",
    characteristic_name = c("Zinc", "Zinc", "Zinc", "Zinc", "Lead"),
    result_sample_fraction = "Total",
    result_analytical_method_id = "200.8",
    result_unit = "ug/L",
    activity_type = c(orig, rep_type, "Sample-Routine", orig, orig),
    qc_status = c("Accepted", "Accepted", "Accepted", "Accepted", "Rejected"),
    qc_fail_reason = c(NA, NA, NA, NA, "Lab QC fail (CCV)"),
    stringsAsFactors = FALSE
  )
}
mk_rpd <- function(qapp = "Fail") {
  data.frame(monitoring_location_id = "1", activity_start_date = "2025-04-30",
             characteristic_name = "Zinc", sample_fraction = "Total", method_id = "200.8",
             unit = "ug/L", rpd_pct = 33.3, threshold = 20, qapp_v3 = qapp,
             stringsAsFactors = FALSE)
}

test_that("a failing duplicate pair rejects the original and the replicate only", {
  out <- finalize_qc_status(mk_export(), mk_rpd("Fail"))
  expect_equal(out$qc_status, c("Rejected", "Rejected", "Accepted", "Accepted", "Rejected"))
  expect_match(out$qc_fail_reason[1], "Field duplicate RPD fail \\(QAPP v3\\): 33.3% exceeds 20%")
  expect_equal(out$flag, c("Y", "Y", "N", "N", "Y"))
})

test_that("passing and not evaluable pairs reject nothing", {
  for (r in c("Pass", "Not evaluable")) {
    out <- finalize_qc_status(mk_export(), mk_rpd(r))
    expect_equal(out$qc_status[1:2], c("Accepted", "Accepted"))
  }
})

test_that("existing lab QC reasons are kept and joined with new ones", {
  ex <- mk_export(); ex$qc_status[1] <- "Rejected"; ex$qc_fail_reason[1] <- "Blank fail"
  out <- finalize_qc_status(ex, mk_rpd("Fail"))
  expect_match(out$qc_fail_reason[1], "^Blank fail; Field duplicate")
  expect_equal(out$qc_fail_reason[5], "Lab QC fail (CCV)")
})

test_that("manual decisions reject matching rows with the note as the reason", {
  man <- data.frame(monitoring_location_id = "2", activity_start_date = as.Date("2025-04-30"),
                    characteristic_name = "Zinc", flag = "Y", notes = "cooler warm",
                    stringsAsFactors = FALSE)
  out <- finalize_qc_status(mk_export(), NULL, man)
  expect_equal(out$qc_status[4], "Rejected")
  expect_equal(out$qc_fail_reason[4], "Manual flag: cooler warm")
})

test_that("a flag with stray spaces or lower case still rejects", {
  man <- data.frame(monitoring_location_id = "2", activity_start_date = as.Date("2025-04-30"),
                    characteristic_name = "Zinc", flag = " y ", notes = "x",
                    stringsAsFactors = FALSE)
  expect_equal(finalize_qc_status(mk_export(), NULL, man)$qc_status[4], "Rejected")
})

test_that("a manual row that matches nothing warns; flag N rows are ignored", {
  man <- data.frame(monitoring_location_id = c("9", "2"),
                    activity_start_date = as.Date("2025-04-30"),
                    characteristic_name = "Zinc", flag = c("Y", "N"), notes = NA_character_,
                    stringsAsFactors = FALSE)
  expect_warning(out <- finalize_qc_status(mk_export(), NULL, man), "matched no result")
  expect_equal(out$qc_status[4], "Accepted")
})

test_that("NULL or empty inputs leave the status unchanged apart from the flag column", {
  out <- finalize_qc_status(mk_export(), NULL, NULL)
  expect_equal(out$qc_status, mk_export()$qc_status)
  expect_equal(out$flag, c("N", "N", "N", "N", "Y"))
  out2 <- finalize_qc_status(mk_export(), mk_rpd()[0, ], mk_rpd()[0, ])
  expect_equal(out2$qc_status, mk_export()$qc_status)
})

test_that("missing columns stop with a message", {
  expect_error(finalize_qc_status(mk_export()[, -1][, -1]), "missing column")
})

test_that("hold time failures are Rejected by dat_row with the hold note as reason", {
  ht <- data.frame(dat_row = c(1, 2), hold_status = c("Fail", "Pass"),
                   hold_note = c("Hold time exceeded: 7.0 h > 6 h", NA),
                   stringsAsFactors = FALSE)
  out <- finalize_qc_status(mk_export(), holding_times = ht)
  expect_equal(out$qc_status[1:2], c("Rejected", "Accepted"))
  expect_match(out$qc_fail_reason[1], "Hold time exceeded")
  expect_equal(out$flag[1], "Y")
  # Reason is appended to an existing lab QC reason
  ht5 <- data.frame(dat_row = 5, hold_status = "Fail", hold_note = "Hold time exceeded",
                    stringsAsFactors = FALSE)
  out5 <- finalize_qc_status(mk_export(), holding_times = ht5)
  expect_equal(out5$qc_fail_reason[5], "Lab QC fail (CCV); Hold time exceeded")
})

test_that("a manual row with a fraction rejects only that fraction; blank rejects all", {
  e <- mk_export()
  e <- rbind(e, transform(e[1, ], dat_row = 6L, result_sample_fraction = "Dissolved",
                          qc_status = "Accepted", qc_fail_reason = NA))
  mf <- data.frame(monitoring_location_id = "1", activity_start_date = as.Date("2025-04-30"),
                   characteristic_name = "Zinc", result_sample_fraction = "Dissolved",
                   flag = "Y", notes = "dissolved > total", stringsAsFactors = FALSE)
  out <- finalize_qc_status(e, manual_flags = mf)
  expect_equal(out$qc_status[out$dat_row == 6], "Rejected")
  expect_equal(out$qc_status[out$dat_row == 1], "Accepted")
  mf$result_sample_fraction <- ""
  out2 <- finalize_qc_status(e, manual_flags = mf)
  expect_equal(out2$qc_status[out2$dat_row %in% c(1, 6)], c("Rejected", "Rejected"))
  mf$result_sample_fraction <- "Total"
  mf$characteristic_name <- "Zinc"
  expect_equal(finalize_qc_status(e, manual_flags = mf)$qc_status[6], "Accepted")
})

test_that("a pair that needs a decision is not auto-rejected; a Y row rejects it", {
  rp <- mk_rpd(); rp$needs_decision <- TRUE
  out <- finalize_qc_status(mk_export(), rpd_pairs = rp)
  expect_equal(out$qc_status[1:2], c("Accepted", "Accepted"))
  mf <- data.frame(monitoring_location_id = "1", activity_start_date = as.Date("2025-04-30"),
                   characteristic_name = "Zinc", result_sample_fraction = "", flag = "Y",
                   notes = "detect vs non-detect", stringsAsFactors = FALSE)
  out2 <- finalize_qc_status(mk_export(), rpd_pairs = rp, manual_flags = mf)
  expect_equal(out2$qc_status[1:2], c("Rejected", "Rejected"))
})
