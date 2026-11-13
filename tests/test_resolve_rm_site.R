# Tests for functions/resolve_rm_site.R (labels are synthetic; sites come from reference/baseline_sites.csv).
library(testthat)
source(here::here("functions", "resolve_rm_site.R"))
sites <- here::here("reference/baseline_sites.csv")

test_that("trip and field blank labels carry the condition and resolve the site", {
  r <- resolve_rm_site(c("RM 22 TRIP BLANK", "TRIP BLANK RM 10", "RM 18 FIELD BLANK"), sites)
  expect_equal(r$sample_condition, c("Trip Blank", "Trip Blank", "Field Blank"))
  expect_equal(r$monitoring_location_id[2], r$monitoring_location_id[which(r$label == "TRIP BLANK RM 10")])
  expect_false(anyNA(r$monitoring_location_id))
})

test_that("a trip blank label with no river mile stops", {
  expect_error(resolve_rm_site("TRIP BLANK", sites), "not matched")
})

test_that("duplicates and unknown river miles behave", {
  expect_equal(resolve_rm_site("RM 22 DUP", sites)$sample_condition, "Field Duplicate")
  expect_error(resolve_rm_site("RM 99", sites), "not matched")
})
