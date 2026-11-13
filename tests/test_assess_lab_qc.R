# Tests for functions/assess_lab_qc.R. Synthetic rows only.
#
# Run: testthat::test_file(here::here("tests/test_assess_lab_qc.R"))

library(testthat)
source(here::here("functions", "assess_lab_qc.R"))

row <- function(qc_group = "accuracy", result = 1, resultflag = "=", repdl = 1,
                percent_recovered = NA_real_, rec_limit_low = NA_real_,
                rec_limit_high = NA_real_, sample_rpd = NA_real_,
                rpd_limit_high = NA_real_) {
  data.frame(qc_group, result, resultflag, repdl, percent_recovered,
             rec_limit_low, rec_limit_high, sample_rpd, rpd_limit_high)
}
assess <- function(...) assess_lab_qc(row(...))

test_that("recovery: inside, on the edge, below and above the limits", {
  expect_equal(assess(percent_recovered = 100, rec_limit_low = 80, rec_limit_high = 120)$qc_result, "Pass")
  expect_equal(assess(percent_recovered = 80, rec_limit_low = 80, rec_limit_high = 120)$qc_result, "Pass")
  expect_equal(assess(percent_recovered = 79, rec_limit_low = 80, rec_limit_high = 120)$qc_result, "Fail")
  expect_equal(assess(percent_recovered = 121, rec_limit_low = 80, rec_limit_high = 120)$qc_result, "Fail")
  expect_equal(assess(percent_recovered = 100, rec_limit_low = 80, rec_limit_high = 120)$qc_check, "Recovery")
})

test_that("a surrogate-style row is assessed by the recovery rule", {
  out <- assess(qc_group = "other", percent_recovered = 60, rec_limit_low = 70, rec_limit_high = 130)
  expect_equal(out$qc_result, "Fail")
})

test_that("missing limits are never a pass", {
  out <- assess(percent_recovered = 100)
  expect_equal(out$qc_result, "Not evaluable")
  expect_equal(out$qc_check, "Recovery")
})

test_that("rows with no applicable check are not evaluable with check None", {
  out <- assess(qc_group = "other")
  expect_equal(c(out$qc_check, out$qc_result), c("None", "Not evaluable"))
})

test_that("RPD above the limit fails; recovery and RPD combine", {
  expect_equal(assess(sample_rpd = 30, rpd_limit_high = 20)$qc_result, "Fail")
  expect_equal(assess(sample_rpd = 10, rpd_limit_high = 20)$qc_result, "Pass")
  both <- assess(percent_recovered = 100, rec_limit_low = 80, rec_limit_high = 120,
                 sample_rpd = 30, rpd_limit_high = 20)
  expect_equal(c(both$qc_check, both$qc_result), c("Recovery+RPD", "Fail"))
})

test_that("recovery passes and RPD has no limit: row passes on the evaluated check", {
  out <- assess(percent_recovered = 100, rec_limit_low = 80, rec_limit_high = 120, sample_rpd = 5)
  expect_equal(out$qc_result, "Pass")
})

test_that("blank: non-detect passes; detected at or above the reporting limit fails", {
  expect_equal(assess(qc_group = "blank", result = 0, resultflag = "U")$qc_result, "Pass")
  expect_equal(assess(qc_group = "blank", result = 0.5, resultflag = "J", repdl = 1)$qc_result, "Pass")
  expect_equal(assess(qc_group = "blank", result = 1, resultflag = "=", repdl = 1)$qc_result, "Fail")
  expect_equal(assess(qc_group = "blank", result = 2, resultflag = "=", repdl = 1)$qc_result, "Fail")
  expect_equal(assess(qc_group = "blank", result = 2, resultflag = "=", repdl = 1)$qc_check, "Blank")
})

test_that("blank with missing result or reporting limit is not evaluable", {
  expect_equal(assess(qc_group = "blank", result = NA_real_, resultflag = "=")$qc_result, "Not evaluable")
  expect_equal(assess(qc_group = "blank", result = 2, resultflag = "=", repdl = NA_real_)$qc_result, "Not evaluable")
})

test_that("missing required columns stop with a message", {
  expect_error(assess_lab_qc(data.frame(qc_group = "blank")), "missing column")
})
