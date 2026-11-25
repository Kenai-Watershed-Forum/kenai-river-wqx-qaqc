library(testthat)
source(here::here("functions/check_wqx_domain.R"))

test_that("check_wqx_domain flags NA, blank and misspelled values", {
  d <- withr::local_tempdir()
  writeLines(c("Domain,Unique Identifier,Name", "x,1,Water", "x,2,Air"), file.path(d, "T.CSV"))
  out <- check_wqx_domain(c("Water", "water", NA, "", "Air"), "T", dir = d)
  expect_setequal(out$value, c("water", "<NA>", ""))
  expect_equal(sum(out$n_rows), 3)
})

test_that("Characteristic list keeps only Accepted names", {
  d <- withr::local_tempdir()
  writeLines(c("Domain,Name,Domain Value Status", "c,Lead,Accepted", "c,Old,Deprecated"), file.path(d, "Characteristic.CSV"))
  expect_equal(read_wqx_domain("Characteristic", d), "Lead")
})

test_that("shipped ActivityMedia list contains Water", {
  expect_true("Water" %in% read_wqx_domain("ActivityMedia"))
})
