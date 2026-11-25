# Tests for akt_zone() in functions/clock_utils.R. Synthetic dates only.
#
# Run: testthat::test_file(here::here("tests/test_clock_utils_akt_zone.R"))

library(testthat)
source(here::here("functions", "clock_utils.R"))

test_that("summer dates are AKDT and winter dates are AKST", {
  expect_equal(akt_zone(as.Date(c("2024-07-15", "2024-01-15"))), c("AKDT", "AKST"))
})

test_that("transition days switch on the correct Sunday", {
  # 2024: DST began Sunday March 10 and ended Sunday November 3
  expect_equal(akt_zone(as.Date(c("2024-03-09", "2024-03-10", "2024-11-02", "2024-11-03"))),
               c("AKST", "AKDT", "AKDT", "AKST"))
})

test_that("character dates work and NA stays NA", {
  expect_equal(akt_zone(c("2025-04-30", NA)), c("AKDT", NA))
})
