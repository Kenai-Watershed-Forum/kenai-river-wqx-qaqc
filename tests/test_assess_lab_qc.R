# Tests for functions/assess_lab_qc.R. Synthetic rows only.
#
# Run: testthat::test_file(here::here("tests/test_assess_lab_qc.R"))

library(testthat)
source(here::here("functions", "assess_lab_qc.R"))

row <- function(qc_group = "accuracy", result = 1, resultflag = "=", loq = 10, dl = 3,
                percent_recovered = NA_real_, rec_limit_low = NA_real_,
                rec_limit_high = NA_real_, sample_rpd = NA_real_,
                rpd_limit_high = NA_real_) {
  data.frame(qc_group, result, resultflag, loq, dl, percent_recovered,
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

test_that("blank: U/ND passes both rules", {
  out <- assess(qc_group = "blank", result = 3, resultflag = "U")
  expect_equal(c(out$qc_result, out$blank_qapp, out$blank_adec), rep("Pass", 3))
  expect_equal(assess(qc_group = "blank", result = 3, resultflag = "ND")$blank_qapp, "Pass")
})

test_that("blank: J detection between DL and LOQ fails QAPP, passes ADEC", {
  out <- assess(qc_group = "blank", result = 5, resultflag = "J", loq = 10, dl = 3)
  expect_equal(c(out$qc_result, out$blank_qapp, out$blank_adec), c("Fail", "Fail", "Pass"))
  expect_equal(out$qc_check, "Blank")
})

test_that("blank: detection at or above LOQ fails both rules", {
  for (r in c(10, 20)) {
    out <- assess(qc_group = "blank", result = r, resultflag = "=", loq = 10, dl = 3)
    expect_equal(c(out$blank_qapp, out$blank_adec), c("Fail", "Fail"))
  }
})

test_that("blank: missing flag falls back to result vs DL", {
  expect_equal(assess(qc_group = "blank", result = 2, resultflag = NA_character_, dl = 3)$blank_qapp, "Pass")
  expect_equal(assess(qc_group = "blank", result = 3, resultflag = NA_character_, dl = 3)$blank_qapp, "Pass")
  expect_equal(assess(qc_group = "blank", result = 4, resultflag = NA_character_, dl = 3)$blank_qapp, "Fail")
  expect_equal(assess(qc_group = "blank", result = 4, resultflag = NA_character_, dl = NA_real_)$blank_qapp, "Not evaluable")
})

test_that("blank: missing result or LOQ is not evaluable", {
  expect_equal(assess(qc_group = "blank", result = NA_real_, resultflag = "=")$qc_result, "Not evaluable")
  out <- assess(qc_group = "blank", result = 5, resultflag = "J", loq = NA_real_)
  expect_equal(c(out$blank_qapp, out$blank_adec), c("Fail", "Not evaluable"))
})

test_that("non-blank rows have NA in the blank columns", {
  out <- assess(percent_recovered = 100, rec_limit_low = 80, rec_limit_high = 120)
  expect_true(is.na(out$blank_qapp) && is.na(out$blank_adec))
})

test_that("missing required columns stop with a message", {
  expect_error(assess_lab_qc(data.frame(qc_group = "blank")), "missing column")
})
