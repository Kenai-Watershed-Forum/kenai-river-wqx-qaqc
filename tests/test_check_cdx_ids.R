# Tests for functions/check_cdx_ids.R. Synthetic sites only; nothing depends on a
# particular year's data or on the real site count.
#
# Run: testthat::test_file(here::here("tests/test_check_cdx_ids.R"))

library(testthat)
source(here::here("functions", "check_cdx_ids.R"))

sites <- data.frame(
  monitoring_location_id = c("10000001", "10000002"),
  site_code = c("KBL_m_01.0", "KBL_t_02.0")
)
results <- data.frame(
  `Monitoring Location ID` = c("10000001", "10000002", "10000002"),
  `Activity ID` = c("KBL_m_01.0-2031-05-01-Cu", "KBL_t_02.0-2031-05-01-Cu", "KBL_t_02.0-2031-05-01-Cu-DUP"),
  check.names = FALSE
)
station <- data.frame(MonitoringLocationIdentifier = c("10000001", "10000002"))

test_that("clean inputs pass", {
  expect_true(check_cdx_ids(results, station, sites))
})

test_that("numeric location IDs are accepted", {
  r <- results
  r[["Monitoring Location ID"]] <- as.numeric(r[["Monitoring Location ID"]])
  expect_true(check_cdx_ids(r, station, sites))
})

test_that("prefixed IDs are rejected in each file", {
  r <- results; r[["Monitoring Location ID"]][1] <- "KENAI_WQX-10000001"
  expect_error(check_cdx_ids(r, station, sites), "Monitoring Location ID. starts with KENAI_WQX")
  r <- results; r[["Activity ID"]][1] <- "KENAI_WQX-KBL_m_01.0-2031-05-01-Cu"
  expect_error(check_cdx_ids(r, station, sites), "Activity ID. starts with KENAI_WQX")
  s <- station; s$MonitoringLocationIdentifier[1] <- "KENAI_WQX-10000001"
  expect_error(check_cdx_ids(results, s, sites), "station .MonitoringLocationIdentifier. starts with KENAI_WQX")
})

test_that("unknown location ID is rejected", {
  r <- results; r[["Monitoring Location ID"]][2] <- "10009999"
  expect_error(check_cdx_ids(r, station, sites), "not in the known site list")
})

test_that("a site code used as the Location ID is rejected", {
  r <- results; r[["Monitoring Location ID"]][1] <- "KBL_m_01.0"
  expect_error(check_cdx_ids(r, station, sites), "not in the known site list")
})

test_that("station file must match the known sites exactly", {
  expect_error(check_cdx_ids(results, station[1, , drop = FALSE], sites), "missing sites")
  s <- rbind(station, data.frame(MonitoringLocationIdentifier = "10000003"))
  expect_error(check_cdx_ids(results, s, sites), "not in the known list")
})

test_that("Activity ID must start with the site code of its own Location ID", {
  r <- results; r[["Activity ID"]][1] <- "RM 1 - Some Site-2031-05-01-Cu"
  expect_error(check_cdx_ids(r, station, sites), "does not start with its site code")
  r <- results; r[["Activity ID"]][1] <- "KBL_t_02.0-2031-05-01-Cu"
  expect_error(check_cdx_ids(r, station, sites), "does not start with its site code")
})

test_that("NA components in an Activity ID are rejected", {
  r <- results; r[["Activity ID"]][1] <- "KBL_m_01.0-2031-05-01-NA"
  expect_error(check_cdx_ids(r, station, sites), "NA component")
  r <- results; r[["Activity ID"]][1] <- "KBL_m_01.0-2031-05-01-Cu-NA"
  expect_error(check_cdx_ids(r, station, sites), "NA component")
  r <- results; r[["Activity ID"]][1] <- NA_character_
  expect_error(check_cdx_ids(r, station, sites), "does not start with its site code")
})

test_that("all problems are reported together", {
  r <- results; r[["Monitoring Location ID"]][1] <- "KENAI_WQX-10000001"; r[["Activity ID"]][3] <- "KBL_t_02.0-2031-05-01-NA"
  err <- tryCatch(check_cdx_ids(r, station, sites), error = conditionMessage)
  expect_match(err, "starts with KENAI_WQX")
  expect_match(err, "NA component")
})

test_that("missing columns give a clear error", {
  expect_error(check_cdx_ids(results[, 1, drop = FALSE], station, sites), "missing column")
})
