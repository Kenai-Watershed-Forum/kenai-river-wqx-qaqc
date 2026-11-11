library(testthat)

source(here::here("functions", "pdf_extract", "extract_ars_pdf.R"))
source(here::here("functions", "pdf_extract", "map_ars_to_template.R"))

narrative_page <- function(receipt_lines) {
  list(
    c("Page one", "Date Received: 4/24/2018"),
    c("Case Narrative", "Work Order: A1800001", "", "SAMPLE RECEIPT:", "", receipt_lines, "",
      "REVIEW FOR COMPLIANCE WITH QA PLAN:", "Nothing to report."),
    c("Detailed Analytical Report", "Workorder (SDG): A1800001")
  )
}
cover <- list(workorder = "A1800001", date_received = "4/24/2018")

test_that("primary receipt with inline temperature and a parenthetical parses", {
  pg <- narrative_page(c("One (1) sample was received on 4/24/2018 11:37:00 AM at a temperature of 6.9\u00b0C (from site)",
                         "at AAA - Anchorage. The sample was received in good condition and in order per chain of",
                         "custody."))
  r <- ars_narrative(pg, "x.pdf", cover)$sample_receipt
  expect_equal(nrow(r), 1)
  expect_equal(r$role, "primary")
  expect_equal(r$receiving_lab, "AAA - Anchorage")
  expect_equal(r$temperature_c, 6.9)
  expect_equal(r$receipt_time, "11:37:00")
  expect_true(is.na(r$date_check))
})

test_that("subcontract receipts and a separate temperature sentence parse", {
  pg <- narrative_page(c(
    "Three (3) samples were received 4/24/2018 10:52 AM at ARS Aleut Analytical - Anchorage.",
    "Samples were received in Anchorage at a temperature of 4.3\u00baC. The samples were received",
    "in good condition and in order per chain of custody.", "",
    "Samples requiring metals analyses were subcontracted to Test America - Denver and arrived",
    "4/27/2018 9:10 AM and at a temperature of 16.7\u00baC."))
  r <- ars_narrative(pg, "x.pdf", cover)$sample_receipt
  expect_equal(r$role, c("primary", "subcontract"))
  expect_equal(r$temperature_c, c(4.3, 16.7))
  expect_equal(r$receiving_lab[2], "Test America - Denver")
  expect_match(r$condition_text[1], "good condition")
})

test_that("a narrative date that disagrees with the cover page uses the cover date and says so", {
  pg <- narrative_page("Three (3) samples were received 4/24/2017 10:52 AM at ARS Aleut Analytical - Anchorage.")
  r <- ars_narrative(pg, "x.pdf", cover)$sample_receipt
  expect_equal(r$receipt_date, as.Date("2018-04-24"))
  expect_equal(r$receipt_date_printed, as.Date("2017-04-24"))
  expect_match(r$date_check, "cover date used")
})

test_that("unparseable receipt statements stop the run", {
  bad_sub <- narrative_page(c("Three (3) samples were received 4/24/2018 10:52 AM at ARS Aleut Analytical - Anchorage.",
                              "Metals were subcontracted to a lab sometime later."))
  expect_error(ars_narrative(bad_sub, "x.pdf", cover), "not understood")
  none <- narrative_page("The samples were received.")
  expect_error(ars_narrative(none, "x.pdf", cover), "exactly one primary")
  no_section <- list(c("Case Narrative", "Nothing here."), c("Detailed Analytical Report"))
  expect_error(ars_narrative(no_section, "x.pdf", cover), "SAMPLE RECEIPT")
})

test_that("a sentence that says 'arrived' without a date is kept in the text, not an error", {
  pg <- narrative_page(c("Five (5) samples were received 7/26/2016 12:05 PM at a temperature of 11.1\u00b0C at ARS Aleut",
                         "Analytical - Anchorage. The samples arrived within 24 hours on ice."))
  r <- ars_narrative(pg, "x.pdf", cover)$sample_receipt
  expect_equal(r$temperature_c, 11.1)
  expect_equal(r$receiving_lab, "ARS Aleut Analytical - Anchorage")
})

test_that("lab_qc combines QC, method blank and surrogate rows in the lab_qc_dat naming", {
  ex <- list(
    results = tibble::tibble(source_file = "a.pdf", workorder = "A1", prep_batch = "B1", performing_lab = "Lab X"),
    method_blanks = tibble::tibble(source_file = "a.pdf", workorder = "A1", prep_batch = "B1", performing_lab = "Lab X",
                                   analyte = "Iron", method_id = "200.7", result_value = NA_real_, non_detect = TRUE,
                                   flags = NA_character_, pql = 60, mdl = 20, units = "ug/L",
                                   lab_sample_number = "B1-MB", analysis_date = as.Date("2018-05-01"), source_page = 14L),
    surrogates = tibble::tibble(),
    qc = tibble::tibble(source_file = "a.pdf", source_page = 12L, prep_batch = "B1", qc_type = c("LCS", "MS", "MSD"),
                        method_id = "200.7", analyte = "Iron", parent_sample = c(NA, "P-01", "P-01"), prep_date = as.Date("2018-05-01"),
                        units = "ug/L", samp_result = c(0, 100, 100), qc_result = c(95, 190, 195), spike_level = 100,
                        recovery = c(95, 90, 95), lcl = 85, ucl = 115, rpd = c(NA, 5, 5), rpd_limit = c(NA, 20, 20),
                        flag = NA_character_)
  )
  q <- ars_build_lab_qc(ex)
  expect_equal(sort(unique(q$sample_type)), c("LCS", "MB", "MS", "MSD"))
  expect_true(all(c("percent_recovered", "rec_limit_low", "rec_limit_high", "sample_rpd", "prp_batch", "sdg") %in% names(q)))
  expect_equal(unique(q$lab_name), "Lab X")
  expect_equal(q$sdg[q$sample_type == "LCS"], "A1")
  expect_equal(q$qc_group[q$sample_type == "MB"], "blank")
})
