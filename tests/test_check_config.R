library(testthat)
source(here::here("functions", "check_config.R"))

make_fixture <- function() {
  dir <- tempfile("data_"); dir.create(dir)
  for (f in c("a.xlsx", "b.xlsx", "c.xls", "d.xls", "e.xlsx", "f.xls", "g.xlsx")) file.create(file.path(dir, f))
  config <- list(
    year = 2030,
    sampling = list(spring_date = "2030-04-30", summer_date = "2030-07-23"),
    files = list(
      sgs = list(spring = list(path = "a.xlsx", sheet = "S"), summer = list(path = "b.xlsx", sheet = "S")),
      fc  = list(spring = list(path = "c.xls"), summer = list(path = "d.xls")),
      tss = list(spring = list(path = "e.xlsx", sheet = "S", run_date = "2030-05-01", run_time = "13:58"),
                 summer = list(path = "f.xls", sheet = "S", run_date = "2030-07-25", run_time = "14:00")),
      ysi = list(both = list(path = "g.xlsx"))
    )
  )
  list(config = config, dir = dir)
}

test_that("a complete config passes", {
  x <- make_fixture()
  expect_true(check_config(x$config, x$dir))
})

test_that("blank year, dates and paths are all reported at once", {
  x <- make_fixture()
  x$config$year <- NULL
  x$config$sampling$spring_date <- ""
  x$config$files$sgs$spring$path <- ""
  expect_error(check_config(x$config, x$dir), "year is blank.*spring_date is blank.*sgs\\$spring\\$path is blank")
})

test_that("a missing file, wrong-year date and blank TSS run time are errors", {
  x <- make_fixture()
  x$config$files$fc$summer$path <- "nope.xls"
  x$config$sampling$summer_date <- "2029-07-23"
  x$config$files$tss$spring$run_time <- NULL
  expect_error(check_config(x$config, x$dir), "not found")
  expect_error(check_config(x$config, x$dir), "not in year 2030")
  expect_error(check_config(x$config, x$dir), "run_time is blank")
})

test_that("a missing lab entry and reversed seasons are errors", {
  x <- make_fixture()
  x$config$files$ysi <- NULL
  x$config$sampling$spring_date <- "2030-08-01"
  expect_error(check_config(x$config, x$dir), "files\\$ysi\\$both is missing")
  expect_error(check_config(x$config, x$dir), "before summer_date")
})
