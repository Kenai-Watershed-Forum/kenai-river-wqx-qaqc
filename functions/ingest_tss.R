# ingest_tss.R
#
# Reads one season of Soldotna Wastewater Treatment Plant (SWWTP) total suspended
# solids (TSS) results and returns data that satisfy the `dat` contract
# (reference/dat_contract.md), plus a lab QC table (method blanks).
#
# Usage:
#   config <- yaml::read_yaml(here::here("config.yml"))
#   tss <- ingest_tss("spring", config)
#   tss$dat      # field results (contract columns)
#   tss$lab_qc   # lab method blanks; never goes to the WQX export
#
# Year-specific settings come from config.yml and nothing about a particular year is
# hardcoded here:
#   sampling$<season>_date        collection date (the lab file's date columns are not used)
#   files$tss$<season>            path, sheet, run_date, run_time (required: copy "DATE OF
#                                 ANALYSIS" and "TIME" from the lab sheet header),
#                                 rec_date, rec_time (optional: chain of custody)
#   tss$lod, tss$loq              reporting limits, mg/L (QAPP Table 5)
#
# TSS is analyzed one to a few days after collection, so collect_date and run_date
# differ and neither is derived from the other.

# parse_clock() and parse_clock_text() live in clock_utils.R
if (!exists("parse_clock", mode = "function")) {
  source(here::here("functions", "clock_utils.R"), local = FALSE)
}

ingest_tss <- function(season,
                       config,
                       data_dir = here::here("data"),
                       sites_path = here::here("reference/baseline_sites.csv")) {
  stopifnot(season %in% c("spring", "summer"))
  suppressPackageStartupMessages({
    library(dplyr); library(readxl); library(janitor)
    library(stringr); library(lubridate); library(hms)
  })

  spec <- config$files$tss[[season]]
  if (is.null(spec)) stop("config.yml has no files$tss$", season, call. = FALSE)
  path <- file.path(data_dir, spec$path)
  if (!file.exists(path)) stop("TSS file not found: ", path, call. = FALSE)

  collect_date <- as.Date(config$sampling[[paste0(season, "_date")]])
  if (is.na(collect_date)) stop("config.yml sampling$", season, "_date is missing", call. = FALSE)

  lod <- suppressWarnings(as.numeric(config$tss$lod))
  loq <- suppressWarnings(as.numeric(config$tss$loq))
  if (length(lod) != 1 || length(loq) != 1 || is.na(lod) || is.na(loq) || lod >= loq) {
    stop("config.yml tss$lod and tss$loq must be set, with lod < loq (mg/L).", call. = FALSE)
  }

  run_date <- if (is.null(spec$run_date)) as.Date(NA) else as.Date(spec$run_date)
  run_time <- if (is.null(spec$run_time)) as_hms(NA) else parse_clock_text(spec$run_time)
  if (is.na(run_date) || is.na(run_time)) {
    stop("config.yml files$tss$", season, " needs run_date and run_time. Copy 'DATE OF ",
         "ANALYSIS' and 'TIME' from the header of the lab sheet.", call. = FALSE)
  }

  rec_date <- if (is.null(spec$rec_date)) as.Date(NA) else as.Date(spec$rec_date)
  rec_time <- if (is.null(spec$rec_time)) as_hms(NA) else parse_clock_text(spec$rec_time)
  if (is.na(rec_date) || is.na(rec_time)) {
    warning("TSS ", season, ": receipt date/time not set in config.yml files$tss$", season,
            " (rec_date, rec_time). Copy them from the chain of custody. ",
            "rec_date and rec_time are NA.", call. = FALSE)
    rec_date <- as.Date(NA)
    rec_time <- as_hms(NA)
  }

  # Read everything as text; locate the table by its header row
  raw <- read_excel(path, sheet = spec$sheet, col_names = FALSE, col_types = "text",
                    .name_repair = "minimal")
  hdr_row <- which(str_detect(raw[[1]], regex("^\\s*Sample_Location\\s*$", ignore_case = TRUE)))[1]
  if (is.na(hdr_row)) {
    stop("No 'Sample_Location' header row found in ", basename(path),
         "; this file is not in the SWWTP TSS format.", call. = FALSE)
  }
  tbl <- raw[(hdr_row + 1):nrow(raw), , drop = FALSE]
  names(tbl) <- make_clean_names(unlist(raw[hdr_row, ], use.names = FALSE))
  needed <- c("sample_location", "sample_type", "sample_time", "ml", "dried_wt", "s_s_mg_l")
  if (!all(needed %in% names(tbl))) {
    stop("TSS file ", basename(path), " is missing expected columns: ",
         paste(setdiff(needed, names(tbl)), collapse = ", "), call. = FALSE)
  }
  tbl <- tbl |>
    filter(!is.na(sample_location)) |>
    mutate(sample_location = str_trim(sample_location),
           sample_type     = str_to_upper(str_trim(sample_type)))

  unknown <- !tbl$sample_type %in% c("PS", "MB")
  if (any(unknown)) {
    stop("Unexpected sample_type in ", basename(path), " (expected PS or MB): ",
         paste(unique(paste0(tbl$sample_location[unknown], " [", tbl$sample_type[unknown], "]")),
               collapse = "; "), call. = FALSE)
  }

  # Summer files carry Date_of_Analysis on every row; it must agree with config
  if ("date_of_analysis" %in% names(tbl)) {
    file_dates <- parse_excel_date(tbl$date_of_analysis)
    file_dates <- unique(file_dates[!is.na(file_dates)])
    if (length(file_dates) && any(file_dates != run_date)) {
      stop("TSS file ", basename(path), " has Date_of_Analysis ",
           paste(file_dates, collapse = ", "), " but config.yml files$tss$", season,
           "$run_date is ", run_date, call. = FALSE)
    }
  }

  # Sites not sampled this event have no weights and no sample time
  no_weight <- is.na(tbl$dried_wt)
  no_time   <- is.na(tbl$sample_time)
  not_sampled <- tbl$sample_type == "PS" & no_weight & no_time
  half <- tbl$sample_type == "PS" & (no_weight != no_time)
  if (any(half)) {
    stop("TSS ", season, ": field rows with a weight but no sample time, or the reverse: ",
         paste(tbl$sample_location[half], collapse = "; "), call. = FALSE)
  }
  if (any(not_sampled)) {
    message("TSS ", season, ": dropped ", sum(not_sampled), " site(s) with no weights and no ",
            "sample time (not sampled): ", paste(tbl$sample_location[not_sampled], collapse = "; "))
  }
  tbl <- tbl[!not_sampled, ]

  tbl <- tbl |>
    mutate(result = suppressWarnings(round(as.numeric(s_s_mg_l), 6)),
           ml     = suppressWarnings(as.numeric(ml)))
  bad <- is.na(tbl$result)
  if (any(bad)) {
    stop("Non-numeric TSS results in ", basename(path), ": ",
         paste(unique(paste0(tbl$sample_location[bad], " = ", tbl$s_s_mg_l[bad])), collapse = "; "),
         call. = FALSE)
  }

  is_mb <- tbl$sample_type == "MB"
  flag  <- function(x) case_when(x < lod ~ "U", x < loq ~ "J", TRUE ~ "=")

  lab_qc <- tbl[is_mb, ] |>
    transmute(
      sample_type = "MB",
      qc_group    = "blank",
      analyte     = "Total suspended solids",
      analytical_method = "2540-D",
      result,
      resultflag  = flag(result),
      repdl       = loq,
      units       = "mg/L",
      amount_spiked = NA_real_, initial_conc = NA_real_, percent_recovered = NA_real_,
      rec_limit_low = NA_real_, rec_limit_high = NA_real_, sample_rpd = NA_real_,
      rpd_limit_low = NA_real_, rpd_limit_high = NA_real_,
      anl_batch = NA_character_, prp_batch = NA_character_, sdg = NA_character_,
      lab_sample_id = sample_location,
      sample_id     = sample_location,
      collect_date  = collect_date,
      run_date      = run_date,
      dissolved     = NA_character_,
      lab_name      = "SWWTP",
      season        = season
    )

  field <- tbl[!is_mb, ]
  sites <- resolve_rm_site(field$sample_location, sites_path)
  if (anyNA(sites$monitoring_location_id)) {
    stop("TSS field labels that are lab QC rows but marked PS: ",
         paste(field$sample_location[is.na(sites$monitoring_location_id)], collapse = "; "),
         call. = FALSE)
  }

  dat <- field |>
    mutate(
      monitoring_location_id = sites$monitoring_location_id,
      sample_condition       = sites$sample_condition,
      resultflag             = flag(result),
      # A value below the LOD is a non-detect: no numeric result, flagged "U"
      result                 = if_else(result < lod, NA_real_, result)
    ) |>
    transmute(
      sample       = sample_location,
      lab_sample   = sample_location,
      lab_name     = "SWWTP",
      analyte      = "Total suspended solids",
      result,
      units        = "mg/L",
      resultflag,
      loq          = loq,
      lod          = lod,
      collect_date = collect_date,
      collect_time = parse_clock(sample_time),
      rec_date     = rec_date,
      rec_time     = rec_time,
      run_date     = run_date,
      run_time     = run_time,
      analytical_method = "2540-D",
      epa_analysis_id   = "2540-D",
      context_code = "APHA",
      monitoring_location_id,
      note         = paste0("Sample volume = ", ml, " mL"),
      sample_type  = "PS",
      sample_condition,
      dissolved    = NA_character_,
      result_sample_fraction = NA_character_
    )

  list(dat = dat, lab_qc = lab_qc)
}

# Excel serial number (text) or an ISO / M-D-Y date string
parse_excel_date <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  out <- as.Date(v, origin = "1899-12-30")
  txt <- is.na(v) & !is.na(x)
  if (any(txt)) {
    out[txt] <- as.Date(lubridate::parse_date_time(x[txt], orders = c("Ymd", "mdY", "mdy"),
                                                   quiet = TRUE))
  }
  out
}
