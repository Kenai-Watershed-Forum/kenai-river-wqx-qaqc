# Tests for functions/assign_qc_status.R. Synthetic rows only.
#
# Run: testthat::test_file(here::here("tests/test_assign_qc_status.R"))

library(testthat)
library(dplyr)
source(here::here("functions", "assign_qc_status.R"))

d0 <- as.Date("2025-01-01")

mk_dat <- function(..., resultflag = "=") {
  tibble(lab_name = "SGS", collect_date = d0, dissolved = NA_character_,
         units = "mg/L", resultflag = resultflag, ...)
}
mk_sb <- function(dat, anl = "A1", prp = NA_character_) {
  dat |> distinct(lab_name, lab_sample, collect_date, analyte, analytical_method, dissolved) |>
    mutate(anl_batch = anl, prp_batch = prp)
}
mk_qc <- function(sample_type = "LCS", qc_group = "accuracy", qc_result = "Fail",
                  qc_check = "Recovery", analyte = "Nitrate", analytical_method = "M1",
                  result = 1, units = "mg/L", anl_batch = "A1", prp_batch = NA_character_,
                  lab_sample_id = "Q1", sample_id = "QC1") {
  tibble(sample_type, qc_group, qc_result, qc_check, analyte, analytical_method, result,
         units, anl_batch, prp_batch, lab_name = "SGS", lab_sample_id, sample_id,
         dissolved = NA_character_)
}
run <- function(dat, qc, sb = mk_sb(dat), mult = 5) assign_qc_status(dat, sb, qc, mult)

test_that("a failing blank rejects detected results below the multiple, not at or above it", {
  dat <- mk_dat(lab_sample = c("S1", "S2", "S3", "S4"), analyte = "Nitrate", analytical_method = "M1",
                result = c(4.9, 5, 6, NA), resultflag = c("=", "=", "=", "U"))
  out <- run(dat, mk_qc(sample_type = "MB", qc_group = "blank", result = 1, qc_check = "Blank"))$dat
  expect_equal(out$qc_status, c("Rejected", "Accepted", "Accepted", "Accepted"))
  expect_match(out$qc_fail_reason[1], "Blank fail")
  expect_match(out$qc_fail_reason[1], "5x")
})

test_that("blank scope respects batch, analyte and the multiple setting", {
  dat <- mk_dat(lab_sample = c("S1", "S2", "S3"), analyte = c("Nitrate", "Nitrate", "Zinc"),
                analytical_method = "M1", result = 2)
  sb <- mk_sb(dat) |> mutate(anl_batch = c("A1", "A2", "A1"))
  qc <- mk_qc(sample_type = "MB", qc_group = "blank", result = 1, qc_check = "Blank")
  expect_equal(run(dat, qc, sb)$dat$qc_status, c("Rejected", "Accepted", "Accepted"))
  expect_equal(run(dat, qc, sb, mult = 1)$dat$qc_status, rep("Accepted", 3))
})

test_that("blank and sample units that differ stop the run", {
  dat <- mk_dat(lab_sample = "S1", analyte = "Nitrate", analytical_method = "M1", result = 2)
  qc <- mk_qc(sample_type = "MB", qc_group = "blank", result = 1, units = "ug/L")
  expect_error(run(dat, qc), "different units")
})

test_that("a batch control failure rejects every result of that analyte and method in the batch", {
  dat <- mk_dat(lab_sample = c("S1", "S2", "S3", "S4"),
                analyte = c("Nitrate", "Nitrate", "Nitrate", "Zinc"),
                analytical_method = c("M1", "M1", "M1", "M1"), result = 2)
  sb <- mk_sb(dat) |> mutate(anl_batch = c("A1", "A1", "A2", "A1"))
  out <- run(dat, mk_qc(sample_type = "CCV", qc_group = "calibration"), sb)
  expect_equal(out$dat$qc_status, c("Rejected", "Rejected", "Accepted", "Accepted"))
  expect_equal(c(out$lab_qc_actions$scope, out$lab_qc_actions$n_rejected), c("batch", "2"))
})

test_that("prep batch links when the analytical batch differs", {
  dat <- mk_dat(lab_sample = "S1", analyte = "Nitrate", analytical_method = "M1", result = 2)
  sb <- mk_sb(dat, anl = "A9", prp = "P1")
  out <- run(dat, mk_qc(anl_batch = "A1", prp_batch = "P1"), sb)$dat
  expect_equal(out$qc_status, "Rejected")
})

test_that("a surrogate failing on a control sample rejects the batch by method", {
  dat <- mk_dat(lab_sample = c("S1", "S2"), analyte = c("Benzene", "Toluene"),
                analytical_method = c("V1", "V1"), result = 2)
  qc <- mk_qc(sample_type = "LCS", qc_group = "surrogate", analyte = "Toluene-d8 (surr)",
              analytical_method = "V1")
  expect_equal(run(dat, qc)$dat$qc_status, c("Rejected", "Rejected"))
})

test_that("a surrogate failing on a field sample rejects only that sample", {
  dat <- mk_dat(lab_sample = c("S1", "S1", "S2"), analyte = c("Benzene", "Toluene", "Benzene"),
                analytical_method = "V1", result = 2)
  qc <- mk_qc(sample_type = "PS", qc_group = "surrogate", analyte = "Toluene-d8 (surr)",
              analytical_method = "V1", lab_sample_id = "S1", sample_id = "RM1")
  out <- run(dat, qc)
  expect_equal(out$dat$qc_status, c("Rejected", "Rejected", "Accepted"))
  expect_equal(out$lab_qc_actions$scope, "sample")
})

test_that("an MS failure rejects only the parent sample", {
  dat <- mk_dat(lab_sample = c("1251731006", "1251731007"), analyte = "Nitrate",
                analytical_method = "M1", result = 2)
  qc <- mk_qc(sample_type = "MS", sample_id = "1251731006MS", lab_sample_id = "999")
  out <- run(dat, qc)
  expect_equal(out$dat$qc_status, c("Rejected", "Accepted"))
  expect_equal(c(out$lab_qc_actions$scope, out$lab_qc_actions$n_rejected), c("parent", "1"))
})

test_that("an MS on another client's sample rejects nothing and is listed as unmatched", {
  dat <- mk_dat(lab_sample = "1251731006", analyte = "Nitrate", analytical_method = "M1", result = 2)
  qc <- mk_qc(sample_type = "MSD", sample_id = "1251737001 MSD FOR [WFI3187]")
  out <- run(dat, qc)
  expect_equal(out$dat$qc_status, "Accepted")
  expect_equal(c(out$lab_qc_actions$n_matched, out$lab_qc_actions$n_rejected), c(0, 0))
})

test_that("passing and not evaluable QC rows reject nothing", {
  dat <- mk_dat(lab_sample = "S1", analyte = "Nitrate", analytical_method = "M1", result = 2)
  qc <- bind_rows(mk_qc(qc_result = "Pass"), mk_qc(qc_result = "Not evaluable"))
  out <- run(dat, qc)
  expect_equal(out$dat$qc_status, "Accepted")
  expect_equal(nrow(out$lab_qc_actions), 0)
})

test_that("results with no batch are never matched by batch or blank scopes", {
  dat <- mk_dat(lab_sample = NA_character_, analyte = "pH", analytical_method = "M1", result = 7)
  sb <- tibble(lab_name = character(), lab_sample = character(), collect_date = as.Date(character()),
               analyte = character(), analytical_method = character(), dissolved = character(),
               anl_batch = character(), prp_batch = character())
  qc <- bind_rows(mk_qc(analyte = "pH"), mk_qc(sample_type = "MB", qc_group = "blank", analyte = "pH"))
  expect_equal(run(dat, qc, sb)$dat$qc_status, "Accepted")
})

test_that("reasons from several failures are joined", {
  dat <- mk_dat(lab_sample = "S1", analyte = "Nitrate", analytical_method = "M1", result = 2)
  qc <- bind_rows(mk_qc(sample_type = "CCV", qc_group = "calibration"),
                  mk_qc(sample_type = "MB", qc_group = "blank", result = 1, qc_check = "Blank"))
  reason <- run(dat, qc)$dat$qc_fail_reason
  expect_match(reason, "Lab QC fail \\(CCV")
  expect_match(reason, "; Blank fail")
})

test_that("duplicate batch keys and missing columns stop with a message", {
  dat <- mk_dat(lab_sample = "S1", analyte = "Nitrate", analytical_method = "M1", result = 2)
  expect_error(run(dat, mk_qc(), sb = bind_rows(mk_sb(dat), mk_sb(dat))), "more than one row")
  expect_error(run(dat, mk_qc()[, -1]), "missing column")
})
