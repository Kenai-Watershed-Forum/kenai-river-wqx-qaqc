# ingest_ysi.R
#
# Reads the field-meter file (YSI ProQuatro for temperature, conductivity, DO and pH;
# Hach for turbidity) covering both sampling seasons and returns data that satisfy the
# `dat` contract (reference/dat_contract.md).
#
# Usage:
#   config <- yaml::read_yaml(here::here("config.yml"))
#   ysi <- ingest_ysi(config)
#   ysi$dat        # field results, replicates averaged (contract columns)
#   ysi$lab_qc     # empty; kept so every ingest function returns the same shape
#   ysi$excluded   # implausible replicates removed from dat, for manual review
#
# Year-specific settings come from config.yml and nothing about a particular year is
# hardcoded here:
#   sampling$spring_date, sampling$summer_date   every row's Site Depart Date must match one
#   files$ysi$both$path                           the single field-meter file
#   ysi$ph_min, ysi$ph_max, ysi$do_max            plausibility limits
# Site names resolve through reference/ysi_site_names.csv. Corrections to reviewed values
# go in year_files/ysi_corrections.csv (columns: site_name, collect_date, parameter,
# replicate, corrected_value, reason). corrected_value is a number, or "keep" to accept an
# implausible value as reported. Corrections are applied to the raw replicates before the
# plausibility check and the averaging.

if (!exists("parse_clock", mode = "function")) {
  source(here::here("functions", "clock_utils.R"), local = FALSE)
}

ingest_ysi <- function(config,
                       data_dir = here::here("data"),
                       site_names_path = here::here("reference/ysi_site_names.csv"),
                       corrections_path = here::here("year_files/ysi_corrections.csv")) {
  suppressPackageStartupMessages({
    library(dplyr); library(readxl); library(stringr); library(hms)
  })

  spec <- config$files$ysi$both
  if (is.null(spec)) stop("config.yml has no files$ysi$both", call. = FALSE)
  path <- file.path(data_dir, spec$path)
  if (!file.exists(path)) stop("YSI file not found: ", path, call. = FALSE)

  season_dates <- as.Date(c(config$sampling$spring_date, config$sampling$summer_date))
  if (anyNA(season_dates)) {
    stop("config.yml sampling$spring_date and sampling$summer_date must both be set", call. = FALSE)
  }

  ph_min <- suppressWarnings(as.numeric(config$ysi$ph_min))
  ph_max <- suppressWarnings(as.numeric(config$ysi$ph_max))
  do_max <- suppressWarnings(as.numeric(config$ysi$do_max))
  if (anyNA(c(ph_min, ph_max, do_max)) || length(c(ph_min, ph_max, do_max)) != 3) {
    stop("config.yml ysi$ph_min, ysi$ph_max and ysi$do_max must be set", call. = FALSE)
  }

  # ---- Parameter definitions ----------------------------------------------------------
  params <- tibble::tibble(
    parameter  = c("Temperature", "Conductivity", "DO", "Turbidity", "pH"),
    analyte    = c("Water Temperature", "Specific Conductance", "Dissolved Oxygen",
                   "Turbidity", "pH"),
    method     = c("170.1", "120.1", "360.1", "180.1", "150.1"),
    units      = c("deg C", "uS/cm", "mg/L", "NTU", "None"),
    file_units = list(c("C", "deg C"), "uS/cm", "mg/L", "NTU", c("None", "su", "SU"))
  )

  # ---- Read ---------------------------------------------------------------------------
  raw <- read_excel(path)
  needed <- c("Site Depart Date", "Site Name", "Meter Model", "Collection Time (HH:MM)",
              "Replicate Observation", "Parameter", "Value", "Unit")
  if (!all(needed %in% names(raw))) {
    stop("YSI file ", basename(path), " is missing expected columns: ",
         paste(setdiff(needed, names(raw)), collapse = ", "), call. = FALSE)
  }

  # Empty template rows have no site, parameter or value
  tbl <- raw |>
    filter(!(is.na(`Site Name`) & is.na(Parameter) & is.na(Value))) |>
    mutate(
      row_id       = row_number(),
      site_name    = str_trim(`Site Name`),
      parameter    = str_trim(Parameter),
      collect_date = as.Date(`Site Depart Date`),
      replicate    = as.numeric(`Replicate Observation`),
      value        = as.numeric(Value),
      collect_time = if (inherits(`Collection Time (HH:MM)`, "POSIXt")) {
        as_hms(format(`Collection Time (HH:MM)`, "%H:%M:%S"))
      } else {
        parse_clock(`Collection Time (HH:MM)`)
      }
    )

  if (anyNA(tbl$site_name) || anyNA(tbl$parameter)) {
    stop("YSI file has rows with a value but no site name or parameter (row(s) ",
         paste(tbl$row_id[is.na(tbl$site_name) | is.na(tbl$parameter)], collapse = ", "), ")",
         call. = FALSE)
  }
  bad_param <- setdiff(unique(tbl$parameter), params$parameter)
  if (length(bad_param)) {
    stop("Unrecognized YSI parameter(s): ", paste(bad_param, collapse = ", "),
         ". Expected: ", paste(params$parameter, collapse = ", "), call. = FALSE)
  }
  bad_date <- tbl |> filter(is.na(collect_date) | !collect_date %in% season_dates)
  if (nrow(bad_date)) {
    stop("YSI Site Depart Date does not match config.yml sampling dates (",
         paste(season_dates, collapse = ", "), ") for: ",
         paste(unique(paste0(bad_date$site_name, " [", bad_date$collect_date, "]")),
               collapse = "; "), call. = FALSE)
  }

  # ---- Site lookup (stops on unmapped names) -------------------------------------------
  lkp <- utils::read.csv(site_names_path, colClasses = "character", na.strings = "")
  unmapped <- setdiff(unique(tbl$site_name), lkp$ysi_site_name)
  if (length(unmapped)) {
    stop("YSI site name(s) not in ", basename(site_names_path), ": ",
         paste(unmapped, collapse = "; "), ". Add a row for each.", call. = FALSE)
  }
  tbl <- tbl |>
    left_join(lkp, by = c("site_name" = "ysi_site_name")) |>
    left_join(params, by = "parameter")

  # ---- Units check ----------------------------------------------------------------------
  unit_ok <- mapply(function(u, ok) is.na(u) || u %in% ok, tbl$Unit, tbl$file_units)
  bad_units <- tbl[!is.na(tbl$value) & !unit_ok, ]
  if (nrow(bad_units)) {
    stop("Unexpected units in YSI file: ",
         paste(unique(paste0(bad_units$parameter, " = ", bad_units$Unit)), collapse = "; "),
         call. = FALSE)
  }

  # ---- Sites with no values (not sampled / no probe) -----------------------------------
  no_vals <- tbl |>
    group_by(site_name, collect_date) |>
    summarise(any_val = any(!is.na(value)), .groups = "drop") |>
    filter(!any_val)
  if (nrow(no_vals)) {
    message("YSI: no values for ", nrow(no_vals), " site visit(s), skipped: ",
            paste0(no_vals$site_name, " [", no_vals$collect_date, "]", collapse = "; "))
  }
  tbl <- tbl |> filter(!is.na(value))

  # Collection Time is recorded only on the turbidity rows; it applies to every parameter
  # at that site visit. Observation Time (when each meter reading was taken) is not used.
  tbl <- tbl |>
    group_by(site_name, collect_date) |>
    mutate(collect_time = {
      t <- unique(collect_time[!is.na(collect_time)])
      if (length(t) > 1) stop("More than one Collection Time for ", first(site_name), " ",
                              first(collect_date), call. = FALSE)
      if (length(t) == 0) as_hms(NA) else t
    }) |>
    ungroup()

  # Meter model: fill blank cells from the same site, date and parameter
  tbl <- tbl |>
    group_by(site_name, collect_date, parameter) |>
    mutate(meter = {
      m <- unique(`Meter Model`[!is.na(`Meter Model`)])
      if (length(m) > 1) stop("More than one meter model for ", first(site_name), " ",
                              first(collect_date), " ", first(parameter), call. = FALSE)
      if (length(m) == 0) NA_character_ else m
    }) |>
    ungroup()
  if (anyNA(tbl$meter)) {
    stop("No Meter Model for: ",
         paste(unique(paste(tbl$site_name[is.na(tbl$meter)], tbl$collect_date[is.na(tbl$meter)],
                            tbl$parameter[is.na(tbl$meter)])), collapse = "; "), call. = FALSE)
  }

  # ---- Corrections ------------------------------------------------------------------------
  tbl$status <- "as reported"
  if (file.exists(corrections_path)) {
    corr <- utils::read.csv(corrections_path, colClasses = "character", na.strings = "")
    corr_cols <- c("site_name", "collect_date", "parameter", "replicate", "corrected_value", "reason")
    if (!all(corr_cols %in% names(corr))) {
      stop(basename(corrections_path), " needs columns: ", paste(corr_cols, collapse = ", "),
           call. = FALSE)
    }
    if (nrow(corr)) {
      corr$collect_date <- as.Date(corr$collect_date)
      corr$replicate <- as.numeric(corr$replicate)
      is_keep <- tolower(trimws(corr$corrected_value)) == "keep"
      new_val <- suppressWarnings(as.numeric(corr$corrected_value))
      if (any(!is_keep & is.na(new_val))) {
        stop("corrected_value in ", basename(corrections_path),
             " must be a number or 'keep'", call. = FALSE)
      }
      if (any(is.na(corr$reason))) {
        stop("Every row in ", basename(corrections_path), " needs a reason", call. = FALSE)
      }
      for (k in seq_len(nrow(corr))) {
        hit <- which(tbl$site_name == corr$site_name[k] & tbl$collect_date == corr$collect_date[k] &
                       tbl$parameter == corr$parameter[k] & tbl$replicate == corr$replicate[k])
        if (length(hit) != 1) {
          stop("Correction row ", k, " in ", basename(corrections_path), " matches ", length(hit),
               " data rows (needs exactly 1): ", corr$site_name[k], ", ", corr$collect_date[k],
               ", ", corr$parameter[k], ", replicate ", corr$replicate[k], call. = FALSE)
        }
        if (is_keep[k]) {
          tbl$status[hit] <- "kept"
        } else {
          tbl$value[hit]  <- new_val[k]
          tbl$status[hit] <- "corrected"
        }
      }
      message("YSI: applied ", nrow(corr), " correction(s) from ", basename(corrections_path))
    }
  } else {
    message("YSI: no corrections file at ", corrections_path, "; none applied")
  }

  # ---- Plausibility ---------------------------------------------------------------------
  tbl <- tbl |>
    mutate(reason = case_when(
      status != "as reported" ~ NA_character_,
      parameter == "pH" & value < ph_min ~ paste0("pH below ", ph_min),
      parameter == "pH" & value > ph_max ~ paste0("pH above ", ph_max),
      parameter == "DO" & value > do_max ~ paste0("DO above ", do_max, " mg/L"),
      TRUE ~ NA_character_
    ))
  excluded <- tbl |>
    filter(!is.na(reason)) |>
    transmute(site_name, collect_date, parameter, replicate, value, reason)
  if (nrow(excluded)) {
    message("YSI: removed ", nrow(excluded), " implausible replicate(s); see $excluded")
  }

  # ---- Average replicates ------------------------------------------------------------------
  dat <- tbl |>
    group_by(monitoring_location_id, sample_condition, analyte, method, units,
             collect_date, meter) |>
    summarise(
      result       = round(mean(value[is.na(reason)]), 6),
      n_total      = n(),
      n_used       = sum(is.na(reason)),
      collect_time = first(collect_time),
      .groups = "drop"
    ) |>
    filter(n_used > 0)

  if (anyNA(dat$collect_time)) {
    stop("No collection time for: ",
         paste(unique(paste(dat$monitoring_location_id[is.na(dat$collect_time)],
                            dat$collect_date[is.na(dat$collect_time)])), collapse = "; "),
         call. = FALSE)
  }

  dat <- dat |>
    transmute(
      sample       = monitoring_location_id,
      lab_sample   = NA_character_,
      lab_name     = meter,
      analyte,
      result,
      units,
      resultflag   = "=",
      loq          = NA_real_,
      lod          = NA_real_,
      collect_date,
      collect_time,
      rec_date     = as.Date(NA),
      rec_time     = as_hms(NA),
      run_date     = collect_date,
      run_time     = collect_time,
      analytical_method = method,
      epa_analysis_id   = method,
      context_code = "USEPA",
      monitoring_location_id,
      note         = if_else(n_used == n_total,
                             paste0("Mean of ", n_used, " replicate(s)"),
                             paste0("Mean of ", n_used, " of ", n_total, " replicate(s)")),
      sample_type  = "PS",
      sample_condition,
      dissolved    = NA_character_,
      result_sample_fraction = "Total"
    ) |>
    arrange(collect_date, monitoring_location_id, analyte)

  list(dat = dat, lab_qc = tibble::tibble(), excluded = excluded)
}
