library(testthat)
source(file.path("..", "functions", "assess_rpd.R"))

mk <- function(orig, rep, loq = 2, ch = "Zinc") {
  row <- function(type, v) data.frame(
    `Monitoring Location ID` = "1", `Activity Start Date` = as.Date("2025-01-01"),
    `Characteristic Name` = ch, `Result Sample Fraction` = "Total",
    `Result Analytical Method ID` = "M", `Result Unit` = "ug/L",
    `Activity Type` = type, `Result Value` = v,
    `Result Detection Limit Value 1` = loq, check.names = FALSE)
  rbind(row("Field Msr/Obs", orig), row("Quality Control Field Replicate Msr/Obs", rep))
}

test_that("both results at or above 2x LOQ: both rules compute RPD", {
  r <- assess_rpd(mk(10, 11))
  expect_equal(c(r$qapp_v3, r$adec_2026), c("Pass", "Pass"))
  r <- assess_rpd(mk(10, 20))
  expect_equal(c(r$qapp_v3, r$adec_2026), c("Fail", "Fail"))
})

test_that("both above LOQ but under 2x LOQ: QAPP skips, ADEC evaluates", {
  r <- assess_rpd(mk(2.5, 3.5))
  expect_equal(r$qapp_v3, "Not evaluable")
  expect_equal(r$adec_2026, "Fail")
  r <- assess_rpd(mk(2.5, 2.6))
  expect_equal(r$adec_2026, "Pass")
})

test_that("one below LOQ: under 2x skipped by ADEC, at least 2x fails by ADEC", {
  r <- assess_rpd(mk(0, 3))
  expect_equal(c(r$qapp_v3, r$adec_2026), c("Not evaluable", "Not evaluable"))
  r <- assess_rpd(mk(0, 5))
  expect_equal(r$adec_2026, "Fail")
  expect_equal(r$qapp_v3, "Fail")
})

test_that("both below LOQ, or missing LOQ, is Not evaluable", {
  r <- assess_rpd(mk(0, 0))
  expect_equal(c(r$qapp_v3, r$adec_2026), c("Not evaluable", "Not evaluable"))
  r <- assess_rpd(mk(10, 11, loq = NA))
  expect_equal(c(r$qapp_v3, r$adec_2026), c("Not evaluable", "Not evaluable"))
})

test_that("thresholds by analyte", {
  expect_equal(assess_rpd(mk(10, 12.5, ch = "Zinc"))$threshold, 20)
  expect_equal(assess_rpd(mk(10, 11, ch = "Total suspended solids"))$qapp_v3, "Fail")
  expect_equal(assess_rpd(mk(10, 12, ch = "Total Phosphorus"))$threshold, 25)
})

test_that("unpaired rows are dropped", {
  d <- mk(10, 11)
  expect_equal(nrow(assess_rpd(d[1, ])), 0)
})
