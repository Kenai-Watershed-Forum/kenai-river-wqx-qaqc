# Tests for functions/check_cdx_fractions.R. Synthetic inputs only.
#
# Run: testthat::test_file(here::here("tests/test_check_cdx_fractions.R"))

library(testthat)
source(here::here("functions", "check_cdx_fractions.R"))

fractions <- utils::read.csv(
  here::here("reference/wqx_templates/wqx_sample_fraction_codes.csv"),
  colClasses = "character"
)$sample_fraction

mk <- function(frac) {
  data.frame(
    `Result Sample Fraction` = frac,
    `Characteristic Name` = "Zinc",
    `Result Analytical Method ID` = "200.8",
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

test_that("the snapshot loads and has no NA", {
  expect_gt(length(fractions), 0)
  expect_false(anyNA(fractions))
})

test_that("valid fractions pass and the input is returned", {
  d <- mk(c("Total", "Dissolved", "Suspended", "None"))
  expect_no_error(out <- check_cdx_fractions(d, fractions))
  expect_identical(out, d)
})

test_that("NA and empty fractions stop", {
  expect_error(check_cdx_fractions(mk(c("Total", NA)), fractions), "missing")
  expect_error(check_cdx_fractions(mk(c("Total", "")), fractions), "missing")
  expect_error(check_cdx_fractions(mk(c("Total", "  ")), fractions), "missing")
})

test_that("values outside the list stop, case-sensitively", {
  expect_error(check_cdx_fractions(mk("total"), fractions), "not in WQX list")
  expect_error(check_cdx_fractions(mk("Totl"), fractions), "not in WQX list")
})

test_that("the message names the characteristic, method and row count", {
  d <- rbind(mk(NA), mk(NA), mk("Total"))
  expect_error(check_cdx_fractions(d, fractions), "Zinc \\| method 200.8 \\| 2 row")
})

test_that("bad inputs are reported", {
  expect_error(check_cdx_fractions(mk("Total")[, 1, drop = FALSE], fractions), "missing column")
  expect_error(check_cdx_fractions(mk("Total"), character(0)), "allowed fraction list")
})
