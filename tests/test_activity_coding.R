# Tests for functions/activity_coding.R. Synthetic conditions only.
#
# Run: testthat::test_file(here::here("tests/test_activity_coding.R"))

library(testthat)
source(here::here("functions", "activity_coding.R"))

cond <- c(NA, "Field Duplicate", "Field Blank", "Trip Blank", "Trip Blank")
type <- c("PS", "PS", "PS", "TB", "TB")

test_that("field blank gets the FB suffix and its own Activity Type", {
  expect_equal(condition_abbrev("Field Blank"), "FB")
  expect_equal(activity_type("Field Blank", "PS"), "Quality Control Sample-Field Blank")
})

test_that("every condition maps; only unconditioned samples have no suffix", {
  abbr <- condition_abbrev(cond)
  expect_equal(abbr, c(NA, "DUP", "FB", "Blank", "Blank"))
  expect_false(anyNA(abbr[!is.na(cond)]))
  expect_equal(activity_type(cond, type),
               c("Field Msr/Obs", "Quality Control Field Replicate Msr/Obs",
                 "Quality Control Sample-Field Blank",
                 "Quality Control Sample-Trip Blank", "Quality Control Sample-Trip Blank"))
})

test_that("unknown condition on a conditioned row gives NA suffix (caught by check_cdx_ids)", {
  expect_true(is.na(condition_abbrev("Mystery")))
})
