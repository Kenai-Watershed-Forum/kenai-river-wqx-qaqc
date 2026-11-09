# Tests for functions/numeric_utils.R. Synthetic inputs only.
#
# Run: testthat::test_file(here::here("tests/test_numeric_utils.R"))

library(testthat)
source(here::here("functions", "numeric_utils.R"))

test_that("numeric input is returned unchanged", {
  expect_identical(to_numeric_strict(c(1.5, NA), "x"), c(1.5, NA))
})

test_that("clean text converts and blanks stay NA", {
  expect_equal(to_numeric_strict(c("1.2", " 3 ", "", NA, "  ", "1e-2"), "x"),
               c(1.2, 3, NA, NA, NA, 0.01))
})

test_that("a letter O for zero stops with a hint", {
  expect_error(to_numeric_strict(c("1.0", "O.18"), "file A column lod"),
               "file A column lod.*'O.18' \\(did you mean 0.18\\?\\)")
})

test_that("a lowercase l for one stops with a hint", {
  expect_error(to_numeric_strict("l.5", "x"), "did you mean 1.5")
})

test_that("other non-numeric text stops without a hint", {
  for (bad in c("1,5", "<0.5", "ND", "abc")) {
    err <- tryCatch(to_numeric_strict(bad, "x"), error = conditionMessage)
    expect_match(err, "Non-numeric text in x")
    expect_no_match(err, "did you mean")
  }
})

test_that("row labels are reported", {
  expect_error(to_numeric_strict(c("1", "O.2"), "x", labels = c("S1", "S2")), "rows: S2")
})

test_that("long lists are truncated", {
  bad <- paste0("O.", 1:12)
  expect_error(to_numeric_strict(bad, "x"), "and 2 more")
})
