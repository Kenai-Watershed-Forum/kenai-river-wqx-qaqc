library(testthat)
source(here::here("functions", "pdf_extract", "check_template_ready.R"))

cols_path <- here::here("functions", "pdf_extract", "template_columns.csv")
cols <- read.csv(cols_path, stringsAsFactors = FALSE)[[1]]

make_wb <- function(rows) {
  d <- as.data.frame(matrix(NA_character_, nrow = length(rows), ncol = length(cols), dimnames = list(NULL, cols)))
  for (i in seq_along(rows)) for (nm in names(rows[[i]])) d[i, nm] <- rows[[i]][[nm]]
  p <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(KWF_Baseline_EPA_Template = d), p)
  p
}
good <- list(`Lab Sample Number` = "A1-01", `Characteristic Name` = "Zinc", `Activity Type` = "Field Msr/Obs",
             `Result Analytical Method ID` = "200.8", `Result Unit` = "ug/L", `Laboratory Name` = "Lab",
             `Analysis Start Date` = "42500", `Sample Receipt Date` = "42490", `Sample Receipt Time` = "0.5",
             `Result Value` = "3.2")
nd <- modifyList(good, list(`Result Value` = NA, `Result Qualifier` = "U",
                            `Result Detection Condition` = "Not Detected", `Result Detection Limit Value 1` = "1"))

test_that("a clean workbook has no errors", {
  r <- check_template_ready(make_wb(list(good, nd)), columns_path = cols_path)
  expect_false(any(r$severity == "error"))
})

test_that("text results and the old non-detect styles are errors", {
  bad1 <- modifyList(good, list(`Result Value` = "<5"))
  bad2 <- modifyList(good, list(`Result Value` = NA, `Result Qualifier` = "ND"))
  r <- check_template_ready(make_wb(list(bad1, bad2)), columns_path = cols_path)
  expect_true(any(grepl("text", r$check) & r$severity == "error"))
  expect_true(any(grepl("non-detect convention", r$check)))
  expect_true(any(grepl("Qualifier not in", r$check)))
})

test_that("receipt after analysis and a missing receipt date are errors", {
  late <- modifyList(good, list(`Sample Receipt Date` = "42600"))
  none <- modifyList(good, list(`Sample Receipt Date` = NA))
  r <- check_template_ready(make_wb(list(late, none)), columns_path = cols_path)
  expect_true(any(grepl("receipt date after", r$check)))
  expect_true(any(grepl("missing Sample Receipt Date", r$check)))
})

test_that("a column difference is reported and stops further checks", {
  p <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(KWF_Baseline_EPA_Template = data.frame(a = 1)), p)
  r <- check_template_ready(p, columns_path = cols_path)
  expect_equal(nrow(r), 1)
  expect_equal(r$severity, "error")
})
