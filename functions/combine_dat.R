# combine_dat.R
#
# Runs every per-lab ingest function for each season in config.yml, row-binds the results into
# `dat` and `lab_qc`, and validates them with check_dat_contract(). Contains nothing
# year-specific: seasons are taken from the names under config$files$<lab>.
#
# Usage:
#   config <- yaml::read_yaml(here::here("config.yml"))
#   res <- combine_dat(config)
#   dat <- res$dat; lab_qc_dat <- res$lab_qc; ysi_excluded <- res$ysi_excluded
#
# Stops (via check_dat_contract) on any contract error. `ingest_fns` can be overridden with
# stub functions for tests; each must return list(dat, lab_qc[, excluded]).

combine_dat <- function(config,
                        data_dir = here::here("data"),
                        ref_dir = here::here("reference"),
                        ingest_fns = list(sgs = ingest_sgs, fc = ingest_fc,
                                          tss = ingest_tss, ysi = ingest_ysi),
                        stop_on_error = TRUE) {
  suppressPackageStartupMessages(library(dplyr))
  seasons <- function(lab) names(config$files[[lab]])

  by_season <- function(lab, ...) {
    setNames(lapply(seasons(lab), function(s)
      ingest_fns[[lab]](season = s, config = config, data_dir = data_dir, ...)), seasons(lab))
  }
  sgs <- by_season("sgs")
  fc  <- by_season("fc")
  tss <- by_season("tss")
  ysi <- ingest_fns$ysi(config = config, data_dir = data_dir)

  parts <- c(unname(sgs), unname(fc), unname(tss), list(ysi))
  dat <- bind_rows(lapply(parts, `[[`, "dat")) |>
    mutate(monitoring_location_id = as.character(monitoring_location_id))
  lab_qc <- bind_rows(lapply(parts, `[[`, "lab_qc")) |>
    mutate(across(any_of("monitoring_location_id"), as.character))

  issues <- check_dat_contract(dat, config, lab_qc = lab_qc, ref_dir = ref_dir,
                               stop_on_error = stop_on_error)

  list(dat = dat, lab_qc = lab_qc, ysi_excluded = ysi$excluded, issues = issues)
}
