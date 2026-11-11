library(testthat)
source(here::here("functions", "pdf_extract", "extract_ars_pdf.R"))

test_that("LCS/LCSD rows give an LCS and an LCSD record, with or without an RPD", {
  ln8 <- "Phosphorous, Total            ND            0.207 0.207    0.200  0.200               104        103       0.19 75 - 125    25.00"
  r <- ars_qc_row(ln8, "LCS/LCSD")
  expect_equal(vapply(r, `[[`, "", "qc_type"), c("LCS", "LCSD"))
  expect_equal(r[[1]]$recovery, 104)
  expect_equal(r[[2]]$recovery, 103)
  expect_equal(r[[1]]$rpd, 0.19)
  expect_equal(c(r[[1]]$lcl, r[[1]]$ucl), c(75, 125))
  ln7 <- "Benzene                      ND        30.8   31.0    30.0   30.0          103       103         79 - 120    20.00"
  r7 <- ars_qc_row(ln7, "LCS/LCSD")
  expect_length(r7, 2)
  expect_true(is.na(r7[[1]]$rpd))
})

test_that("MS/MSD rows with a blank RPD parse", {
  ln <- "Phosphorous, Total            0.101   0.326     0.352  0.129  0.194   175    130          80 - 120    20"
  r <- ars_qc_row(ln, "MS/MSD")
  expect_equal(vapply(r, `[[`, "", "qc_type"), c("MS", "MSD"))
  expect_true(is.na(r[[1]]$rpd))
})

narrative_pages <- function(receipt) {
  list(c("Case Narrative", "Work Order: A1", "SAMPLE RECEIPT:", receipt, "",
         "REVIEW FOR COMPLIANCE WITH QA PLAN", "text"))
}
cover <- list(workorder = "A1", date_received = "4/29/2014")

test_that("2014 wording: comma-separated temperature and a date-only transfer receipt", {
  txt <- c("Three (3) samples were received on 4/29/2014 6:20:00 PM, at a temperature of 4.6\u00b0C, at Analytica-Anchorage.",
           "The samples were received in good condition.",
           "The samples were transferred for metals analysis to Analytica Environmental Laboratories (AEL), 12189 Pennsylvania St., Thornton, Colorado 80241, where they were received at a temperature of 2.7\u00b0C, in good condition and in order per chain of custody on 5/1/2014.")
  rec <- ars_narrative(narrative_pages(txt), "f.pdf", cover)$sample_receipt
  expect_equal(rec$role, c("primary", "subcontract"))
  expect_equal(rec$temperature_c, c(4.6, 2.7))
  expect_equal(rec$receipt_time[1], "18:20:00")
  expect_true(is.na(rec$receipt_time[2]))
  expect_equal(format(rec$receipt_date[2]), "2014-05-01")
})

test_that("two coolers keep the highest temperature and list both", {
  txt <- "Four (4) samples were received on 4/29/2014 6:20:00 PM, in two coolers, at temperatures of 9.1\u00b0C and 8.7\u00b0C, at Analytica-Anchorage."
  rec <- ars_narrative(narrative_pages(txt), "f.pdf", cover)$sample_receipt
  expect_equal(rec$temperature_c, 9.1)
  expect_equal(rec$temperature_all, "9.1; 8.7")
})

test_that("2015 wording: no receiving lab, temperature in a separate sentence", {
  txt <- "There were 3 samples received on 4/29/2014 6:20:00 PM. Samples were received in goo condition at a temperature of 5.7 deg. C."
  rec <- ars_narrative(narrative_pages(txt), "f.pdf", cover)$sample_receipt
  expect_equal(rec$temperature_c, 5.7)
  expect_true(is.na(rec$receiving_lab))
})

test_that("a receipt sentence that cannot be parsed still stops the run", {
  txt <- "Samples were transferred to a lab somewhere."
  expect_error(ars_narrative(narrative_pages(txt), "f.pdf", cover), "not understood|primary receipt")
})
