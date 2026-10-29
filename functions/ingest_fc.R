# ingest_fc.R
#
# Reads one season of Soldotna Wastewater Treatment Plant (SWWTP) fecal coliform
# results and returns data that satisfy the `dat` contract (reference/dat_contract.md),
# plus a lab QC table (method blanks and positive controls).
#
# Usage:
#   config <- yaml::read_yaml(here::here("config.yml"))
#   fc <- ingest_fc("spring", config)
#   fc$dat      # field results (contract columns)
#   fc$lab_qc   # BLANK / POSITIVE rows (result_text keeps e.g. "TNTC"); never goes to the WQX export
#
# Year-specific settings come from config.yml (file path, sampling date, and the
# receipt date/time from the chain of custody); nothing about a particular year is
# hardcoded here. Fecal coliform is analyzed the day it is collected, so
# run_date == collect_date; run_time is each row's "Time In".

# parse_clock() and parse_clock_text() live in clock_utils.R
if (!exists("parse_clock", mode = "function")) {
  source(here::here("functions", "clock_utils.R"), local = FALSE)
}

ingest_fc <- function(season,
                      config,
                      data_dir = here::here("data"),
                      sites_path = here::here("reference/baseline_sites.csv")) {
  stopifnot(season %in% c("spring", "summer"))
  suppressPackageStartupMessages({
    library(dplyr); library(readxl); library(janitor)
    library(stringr); library(lubridate); library(hms)
  })

  spec <- config$files$fc[[season]]
  if (is.null(spec)) stop("config.yml has no files$fc$", season, call. = FALSE)
  path <- file.path(data_dir, spec$path)
  if (!file.exists(path)) stop("FC file not found: ", path, call. = FALSE)

  collect_date <- as.Date(config$sampling[[paste0(season, "_date")]])
  if (is.na(collect_date)) stop("config.yml sampling$", season, "_date is missing", call. = FALSE)

  # Read everything as text; locate the table by its header row
  raw <- read_excel(path, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  first_col <- raw[[1]]
  hdr_row <- which(str_detect(first_col, regex("^\\s*Dish Number", ignore_case = TRUE)))[1]
  if (is.na(hdr_row)) {
    stop("No 'Dish Number' header row found in ", basename(path),
         "; this file is not in the SWWTP fecal coliform format.", call. = FALSE)
  }

  # Header block: "DATE IN: MM-DD-YY" must agree with the config collection date
  header_text <- unlist(raw[seq_len(hdr_row - 1), ], use.names = FALSE)
  header_text <- header_text[!is.na(header_text)]
  parse_header_date <- function(tag) {
    hit <- str_subset(header_text, regex(paste0("^\\s*", tag, "\\s*:"), ignore_case = TRUE))[1]
    if (is.na(hit)) return(as.Date(NA))
    mdy(str_trim(str_remove(hit, regex(paste0("^\\s*", tag, "\\s*:"), ignore_case = TRUE))),
        quiet = TRUE)
  }
  date_in  <- parse_header_date("DATE IN")
  date_out <- parse_header_date("DATE OUT")
  if (is.na(date_in)) {
    stop("No 'DATE IN' line found in ", basename(path), call. = FALSE)
  }
  if (date_in != collect_date) {
    stop("FC file ", basename(path), " has DATE IN ", date_in,
         " but config.yml sampling$", season, "_date is ", collect_date,
         ". Fecal coliform is run the day it is collected.", call. = FALSE)
  }

  tbl <- raw[(hdr_row + 1):nrow(raw), , drop = FALSE]
  names(tbl) <- make_clean_names(unlist(raw[hdr_row, ], use.names = FALSE))
  needed <- c("dish_number", "sample_location_rm", "ml", "time_sampled", "time_in",
              "time_out", "colony_count_100m_l")
  if (!all(needed %in% names(tbl))) {
    stop("FC file ", basename(path), " is missing expected columns: ",
         paste(setdiff(needed, names(tbl)), collapse = ", "), call. = FALSE)
  }
  tbl <- tbl |>
    filter(!is.na(sample_location_rm)) |>
    mutate(sample_location_rm = str_trim(sample_location_rm))

  # Sites not sampled this event are reported as N/A
  not_sampled <- str_detect(tbl$colony_count_100m_l, regex("^\\s*N/?A\\s*$", ignore_case = TRUE)) |
    is.na(tbl$colony_count_100m_l)
  if (any(not_sampled)) {
    message("FC ", season, ": dropped ", sum(not_sampled), " row(s) with no result (N/A): ",
            paste(tbl$sample_location_rm[not_sampled], collapse = "; "))
  }
  tbl <- tbl[!not_sampled, ]

  sites <- resolve_rm_site(tbl$sample_location_rm, sites_path)

  # Lab QC rows may be text (a positive control is typically "TNTC"); field results may not
  tbl$result_text <- tbl$colony_count_100m_l
  tbl$count <- suppressWarnings(as.numeric(tbl$colony_count_100m_l))
  bad <- is.na(tbl$count) & is.na(sites$lab_qc_type)
  if (any(bad)) {
    stop("Non-numeric FC field results in ", basename(path), ": ",
         paste(unique(tbl$result_text[bad]), collapse = "; "), call. = FALSE)
  }

  tbl <- tbl |>
    mutate(
      monitoring_location_id = sites$monitoring_location_id,
      sample_condition       = sites$sample_condition,
      lab_qc_type            = sites$lab_qc_type,
      time_sampled = parse_clock(time_sampled),
      time_in      = parse_clock(time_in),
      time_out     = parse_clock(time_out)
    )

  # Receipt comes from the chain of custody, which is not in the lab file
  rec_date <- if (is.null(spec$rec_date)) as.Date(NA) else as.Date(spec$rec_date)
  rec_time <- if (is.null(spec$rec_time)) as_hms(NA) else parse_clock_text(spec$rec_time)
  if (is.na(rec_date) || is.na(rec_time)) {
    warning("FC ", season, ": receipt date/time not set in config.yml files$fc$", season,
            " (rec_date, rec_time). Copy them from the chain of custody. ",
            "rec_date and rec_time are NA; Q17 cannot evaluate FC hold times from receipt.",
            call. = FALSE)
    rec_date <- as.Date(NA)
    rec_time <- as_hms(NA)
  }

  common <- tbl |>
    mutate(
      lab_name        = "SWWTP",
      analyte         = "Fecal Coliform",
      units           = "cfu/100ml",
      analytical_method = "9222D",
      epa_analysis_id = "9222D",
      context_code    = "APHA",
      loq             = 1,
      run_date        = collect_date,
      run_time        = time_in,
      end_text        = paste0("analysis end = ",
                               ifelse(is.na(date_out), "", format(date_out)), " ",
                               ifelse(is.na(time_out), "", format(time_out)))
    )

  lab_qc <- common |>
    filter(!is.na(lab_qc_type)) |>
    transmute(
      sample_type = lab_qc_type,
      qc_group    = if_else(lab_qc_type == "MB", "blank", "accuracy"),
      analyte, analytical_method,
      result      = count,
      result_text,
      resultflag  = case_when(is.na(count) ~ NA_character_, count < 1 ~ "U", TRUE ~ "="),
      repdl       = loq,
      units,
      amount_spiked = NA_real_, initial_conc = NA_real_, percent_recovered = NA_real_,
      rec_limit_low = NA_real_, rec_limit_high = NA_real_, sample_rpd = NA_real_,
      rpd_limit_low = NA_real_, rpd_limit_high = NA_real_,
      anl_batch = NA_character_, prp_batch = NA_character_, sdg = NA_character_,
      lab_sample_id = dish_number,
      sample_id     = sample_location_rm,
      collect_date  = collect_date,
      run_date,
      dissolved     = NA_character_,
      lab_name,
      season        = season
    )

  dat <- common |>
    filter(is.na(lab_qc_type)) |>
    mutate(
      # A zero count is a non-detect: no numeric result, flagged "U"
      resultflag = if_else(count < 1, "U", "="),
      result     = na_if(count, 0)
    ) |>
    transmute(
      sample       = sample_location_rm,
      lab_sample   = dish_number,
      lab_name, analyte, result, units, resultflag, loq,
      lod          = NA_real_,
      collect_date = collect_date,
      collect_time = time_sampled,
      rec_date     = rec_date,
      rec_time     = rec_time,
      run_date, run_time,
      analytical_method, epa_analysis_id, context_code,
      monitoring_location_id,
      note         = paste0("Analysis volume = ", ml, " mL; ", end_text),
      sample_type  = "PS",
      sample_condition,
      dissolved    = NA_character_,
      result_sample_fraction = NA_character_
    )

  list(dat = dat, lab_qc = lab_qc)
}
