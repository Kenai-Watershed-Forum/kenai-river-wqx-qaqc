# ingest_sgs.R
#
# Reads one season of SGS EDD results and returns data that satisfy the `dat`
# contract (reference/dat_contract.md), plus a separate lab QC table.
#
# Usage:
#   config <- yaml::read_yaml(here::here("config.yml"))
#   sgs <- ingest_sgs("spring", config)
#   sgs$dat      # field results (contract columns)
#   sgs$lab_qc   # lab QC rows, incl. (surr) analytes; never goes to the WQX export
#
# Year-specific settings (file path, sheet, trip blank site mapping) come from
# config.yml; nothing about a particular year is hardcoded here.

# Lab QC sample-type codes (SGS EDD `sample_type`). TB (trip blank) is field data.
sgs_lab_qc_types <- c("MB", "LCS", "LCSD", "MS", "MSD", "CB", "OS",
                      "ICV", "CCV", "LLQC", "QCS", "ICB", "CVC", "IB")

ingest_sgs <- function(season,
                       config,
                       data_dir      = here::here("data"),
                       templates_dir = here::here("reference/wqx_templates")) {
  stopifnot(season %in% c("spring", "summer"))
  suppressPackageStartupMessages({
    library(dplyr); library(readxl); library(janitor)
    library(stringr); library(lubridate); library(hms)
  })

  spec <- config$files$sgs[[season]]
  if (is.null(spec)) stop("config.yml has no files$sgs$", season, call. = FALSE)
  path <- file.path(data_dir, spec$path)
  if (!file.exists(path)) stop("SGS file not found: ", path, call. = FALSE)

  raw <- read_excel(path, sheet = spec$sheet) |>
    clean_names() |>
    remove_empty(which = c("rows", "cols"))

  # Dates arrive as "MM/DD/YYYY HH:MM" strings
  raw <- raw |>
    mutate(
      collect_dt   = mdy_hm(collect_date),
      collect_date = as.Date(collect_dt),
      collect_time = as_hms(collect_dt),
      rec_dt       = mdy_hm(rec_date),
      rec_date     = as.Date(rec_dt),
      rec_time     = as_hms(rec_dt),
      run_dt       = mdy_hm(run_date_time),
      run_date     = as.Date(run_dt),
      run_time     = as_hms(run_dt),
      # EP200.8 -> 200.8
      analytical_method = str_replace(analytical_method, "^EP", ""),
      # Metals only; nutrients and others are filled by format_wqx.R lookups
      result_sample_fraction = case_when(
        dissolved == "L" & analytical_method == "200.8" ~ "Dissolved",
        dissolved == "." & analytical_method == "200.8" ~ "Unfiltered",
        TRUE ~ NA_character_
      ),
      sample_id = str_trim(sample_id)
    ) |>
    select(-collect_dt, -rec_dt, -run_dt, -run_date_time)

  analysis_codes <- read_excel(file.path(templates_dir, "analysis_code_matching_table.xlsx")) |>
    clean_names() |>
    select(sgs_analysis_code, epa_analysis_id, context_code) |>
    mutate(sgs_analysis_code = str_replace(sgs_analysis_code, "^EP", "")) |>
    rename(analytical_method = sgs_analysis_code)

  site_names <- read_excel(file.path(templates_dir, "sgs_site_names_matching_table.xlsx")) |>
    clean_names() |>
    select(sgs_sample_id, monitoring_location_id, sample_condition) |>
    mutate(sgs_sample_id = str_trim(sgs_sample_id),
           monitoring_location_id = as.character(monitoring_location_id)) |>
    rename(sample_id = sgs_sample_id)

  raw <- raw |>
    left_join(analysis_codes, by = "analytical_method") |>
    left_join(site_names, by = "sample_id")

  # Lab QC: the 14 QC sample types, plus surrogates from any sample type
  is_lab_qc <- raw$sample_type %in% sgs_lab_qc_types |
    str_detect(raw$analyte, fixed("(surr)", ignore_case = TRUE))

  lab_qc <- raw[is_lab_qc, ] |>
    mutate(
      lab_name = "SGS",
      season   = season,
      result   = suppressWarnings(as.numeric(result)),
      qc_group = case_when(
        str_detect(analyte, fixed("(surr)", ignore_case = TRUE)) ~ "surrogate",
        sample_type %in% c("LCS", "LCSD", "MS", "MSD", "QCS")    ~ "accuracy",
        sample_type %in% c("ICV", "CCV", "CVC")                  ~ "calibration",
        sample_type %in% c("MB", "CB", "ICB", "IB")              ~ "blank",
        TRUE                                                     ~ "other"
      )
    ) |>
    select(sample_type, qc_group, analyte, analytical_method, result, resultflag,
           repdl, units, amount_spiked, initial_conc, percent_recovered,
           rec_limit_low, rec_limit_high, sample_rpd, rpd_limit_low, rpd_limit_high,
           anl_batch, prp_batch, sdg, lab_sample_id, sample_id, collect_date,
           run_date, dissolved, lab_name, season)

  dat <- raw[!is_lab_qc, ] |>
    filter(!is.na(result)) |>
    mutate(
      lab_name = "SGS",
      result   = as.numeric(result),
      note     = sdg
    ) |>
    select(
      sample = sample_id, lab_sample = lab_sample_id, lab_name, analyte, result,
      units, resultflag, loq, lod, collect_date, collect_time, rec_date, rec_time,
      run_date, run_time, analytical_method, epa_analysis_id, context_code,
      monitoring_location_id, note, sample_type, sample_condition, dissolved,
      result_sample_fraction
    )

  # Trip blanks cover two sites each; sites are listed in config.yml
  dat <- expand_trip_blanks(dat, config$sgs$trip_blank_sites)

  # Fail loudly rather than carry NAs into the contract
  if (anyNA(dat$monitoring_location_id)) {
    stop("SGS sample IDs with no monitoring_location_id (add to sgs_site_names_matching_table.xlsx ",
         "or config.yml sgs$trip_blank_sites): ",
         paste(unique(dat$sample[is.na(dat$monitoring_location_id)]), collapse = "; "),
         call. = FALSE)
  }
  if (anyNA(dat$epa_analysis_id)) {
    stop("SGS analytical methods with no match in analysis_code_matching_table.xlsx: ",
         paste(unique(dat$analytical_method[is.na(dat$epa_analysis_id)]), collapse = "; "),
         call. = FALSE)
  }
  if (any(grepl("^mg/l$", dat$units, ignore.case = TRUE) & dat$units != "mg/L")) {
    stop("SGS units contain a non-WQX capitalization of mg/L", call. = FALSE)
  }

  list(dat = dat, lab_qc = lab_qc)
}

# Duplicates rows whose `sample` is a multi-site trip blank, one row per covered site.
# `mapping` is the parsed config.yml list: sample label -> vector of location IDs.
expand_trip_blanks <- function(dat, mapping) {
  need <- is.na(dat$monitoring_location_id)
  if (!any(need)) return(dat)
  if (is.null(mapping)) mapping <- list()
  lkp <- tibble::tibble(
    sample = rep(names(mapping), lengths(mapping)),
    monitoring_location_id = as.character(unlist(mapping, use.names = FALSE))
  )
  dplyr::bind_rows(
    dat[!need, ],
    dat[need, ] |>
      dplyr::select(-monitoring_location_id) |>
      dplyr::left_join(lkp, by = "sample", relationship = "many-to-many")
  )
}
