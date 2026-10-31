# check_dat_contract.R
#
# Validates the row-bound `dat` (and optionally `lab_qc_dat`) against
# reference/dat_contract.md. It tests structure and rules only: column names and types,
# allowed values, reference-table membership and consistency with config.yml. It never
# tests row counts, site counts or any year-specific value.
#
# Usage:
#   config <- yaml::read_yaml(here::here("config.yml"))
#   dat <- dplyr::bind_rows(sgs$dat, fc$dat, tss$dat, ysi$dat)
#   check_dat_contract(dat, config)                       # stops on any error
#   issues <- check_dat_contract(dat, config, lab_qc = lab_qc_dat, stop_on_error = FALSE)
#
# Returns (invisibly) a tibble of problems: check, severity, n_rows, labs, examples.
# Zero rows means the data satisfy the contract.

check_dat_contract <- function(dat,
                               config,
                               lab_qc = NULL,
                               ref_dir = here::here("reference"),
                               stop_on_error = TRUE) {
  suppressPackageStartupMessages({ library(dplyr); library(stringr) })

  issues <- list()
  add <- function(check, bad, examples = NULL, severity = "error", data = dat) {
    # `bad` is a logical vector over rows of `data`, or a single TRUE for a table-level problem
    n <- if (length(bad) == nrow(data)) sum(bad, na.rm = TRUE) else sum(bad)
    if (n == 0) return(invisible())
    labs <- if (length(bad) == nrow(data) && "lab_name" %in% names(data)) {
      paste(sort(unique(data$lab_name[which(bad)])), collapse = ", ")
    } else NA_character_
    ex <- if (is.null(examples)) NA_character_ else
      paste(utils::head(unique(as.character(examples)), 5), collapse = "; ")
    issues[[length(issues) + 1]] <<- tibble::tibble(
      check = check, severity = severity, n_rows = as.integer(n), labs = labs, examples = ex)
  }

  # ---- 1. Structure ---------------------------------------------------------------------
  spec <- tibble::tribble(
    ~name,                    ~class,
    "sample",                 "chr",  "lab_sample",             "chr",  "lab_name",               "chr",
    "analyte",                "chr",  "result",                 "dbl",  "units",                  "chr",
    "resultflag",             "chr",  "loq",                    "dbl",  "lod",                    "dbl",
    "collect_date",           "Date", "collect_time",           "hms",  "rec_date",               "Date",
    "rec_time",               "hms",  "run_date",               "Date", "run_time",               "hms",
    "analytical_method",      "chr",  "epa_analysis_id",        "chr",  "context_code",           "chr",
    "monitoring_location_id", "chr",  "note",                   "chr",  "sample_type",            "chr",
    "sample_condition",       "chr",  "dissolved",              "chr",  "result_sample_fraction", "chr"
  )
  is_class <- function(x, cls) switch(cls,
    chr = is.character(x), dbl = is.numeric(x), Date = inherits(x, "Date"), hms = inherits(x, "hms"))

  missing_cols <- setdiff(spec$name, names(dat))
  extra_cols   <- setdiff(names(dat), spec$name)
  add("columns missing", length(missing_cols) > 0, missing_cols)
  add("unexpected extra columns", length(extra_cols) > 0, extra_cols)
  present <- intersect(spec$name, names(dat))
  wrong_type <- present[!mapply(function(n, c) is_class(dat[[n]], c), present,
                                spec$class[match(present, spec$name)])]
  add("wrong column type", length(wrong_type) > 0, wrong_type)

  if (length(missing_cols) || length(wrong_type)) {
    # Later checks assume the structure holds
    return(.finish_contract(issues, stop_on_error))
  }

  # ---- 2. Required values -----------------------------------------------------------------
  no_na <- c("sample", "lab_name", "analyte", "units", "resultflag", "analytical_method",
             "epa_analysis_id", "context_code", "monitoring_location_id", "sample_type",
             "collect_date", "run_date")
  for (n in no_na) add(paste0("NA in required column: ", n), is.na(dat[[n]]))

  # ---- 3. Result and flag -----------------------------------------------------------------
  flags_ok <- c("=", "U", "J", "ND")
  add("resultflag not one of =, U, J, ND", !dat$resultflag %in% flags_ok, dat$resultflag)
  add("result is NA but resultflag is not U or ND",
      is.na(dat$result) & !dat$resultflag %in% c("U", "ND"), dat$analyte)

  # ---- 4. Units ---------------------------------------------------------------------------
  units_check <- function(x, data, label) {
    uom <- utils::read.csv(file.path(ref_dir, "wqx_templates/wqx_uom_codes.csv"),
                           colClasses = "character")$code
    spell <- utils::read.csv(file.path(ref_dir, "wqx_templates/project_unit_spellings.csv"),
                             colClasses = "character")$unit
    ok <- x %in% c(uom, spell)
    add(paste0(label, "unit not in the WQX UoM list or project_unit_spellings.csv"),
        !ok & !is.na(x), x, data = data)
    uniq <- unique(x[!is.na(x)])
    clash <- uniq[duplicated(tolower(uniq)) | duplicated(tolower(uniq), fromLast = TRUE)]
    add(paste0(label, "same unit spelled with different capitalization"),
        x %in% clash, clash, data = data)
  }
  units_check(dat$units, dat, "")

  # ---- 5. Monitoring location -------------------------------------------------------------
  add("monitoring_location_id is not bare digits (a KENAI_WQX- prefix is not allowed)",
      !grepl("^[0-9]+$", dat$monitoring_location_id), dat$monitoring_location_id)
  match_xlsx <- file.path(ref_dir, "wqx_templates/wqx_template_matching_table.xlsx")
  site_ids <- as.character(readxl::read_excel(match_xlsx, sheet = "site_coordinates")$monitoring_location_id)
  add("monitoring_location_id not in site_coordinates",
      grepl("^[0-9]+$", dat$monitoring_location_id) & !dat$monitoring_location_id %in% site_ids,
      dat$monitoring_location_id)

  # ---- 6. Lab QC rows and surrogates must not be in dat -------------------------------------
  lab_qc_types <- c("MB", "LCS", "LCSD", "MS", "MSD", "CB", "OS", "ICV", "CCV", "LLQC",
                    "QCS", "ICB", "CVC", "IB")
  add("lab QC sample_type in dat (belongs in lab_qc_dat)", dat$sample_type %in% lab_qc_types,
      dat$sample_type)
  add("surrogate analyte in dat (belongs in lab_qc_dat)", grepl("(surr)", dat$analyte, fixed = TRUE),
      dat$analyte)

  # ---- 7. Sample condition ----------------------------------------------------------------
  add("sample_condition not NA, Field Duplicate, Field Blank or Trip Blank",
      !is.na(dat$sample_condition) &
        !dat$sample_condition %in% c("Field Duplicate", "Field Blank", "Trip Blank"),
      dat$sample_condition)

  # ---- 8. Lookup tables -------------------------------------------------------------------
  analytes <- utils::read.csv(file.path(ref_dir, "wqx_templates/analytes_list_manual_edit.csv"),
                              colClasses = "character")$analyte
  add("analyte not in analytes_list_manual_edit.csv", !dat$analyte %in% analytes, dat$analyte)
  for (sh in c("result_sample_fraction", "chemical_preservative", "sample_container_type_color")) {
    methods <- readxl::read_excel(match_xlsx, sheet = sh)$analytical_method
    # Warning, not error: format_wqx.R left-joins these sheets, so an unlisted method gets NA
    # (not a stop). Promote to error once the sheets cover every method the labs report.
    add(paste0("analytical_method not in matching table sheet ", sh),
        !dat$analytical_method %in% methods, dat$analytical_method, severity = "warning")
  }

  # ---- 9. Unique result key ---------------------------------------------------------------
  key <- dat |>
    mutate(sample_condition = coalesce(sample_condition, "primary"),
           result_sample_fraction = coalesce(result_sample_fraction, "none")) |>
    select(monitoring_location_id, collect_date, analyte, result_sample_fraction,
           sample_condition, epa_analysis_id)
  add("duplicate result key (location, date, analyte, fraction, condition, method)",
      duplicated(key) | duplicated(key, fromLast = TRUE),
      paste(key$monitoring_location_id, key$collect_date, key$analyte))

  # ---- 10. Consistency with config.yml ----------------------------------------------------
  season_dates <- as.Date(c(config$sampling$spring_date, config$sampling$summer_date))
  add("collect_date is not config sampling$spring_date or sampling$summer_date",
      !dat$collect_date %in% season_dates, dat$collect_date)
  if (!is.null(config$year)) {
    add("collect_date year differs from config year",
        format(dat$collect_date, "%Y") != as.character(config$year), dat$collect_date)
  }
  add("run_date is before collect_date", dat$run_date < dat$collect_date, dat$run_date)
  add("rec_date is before collect_date", dat$rec_date < dat$collect_date, dat$rec_date)
  add("rec_date is after run_date", dat$rec_date > dat$run_date, dat$rec_date)

  # ---- 11. lab_qc_dat (minimal until its contract is written) ----------------------------------
  if (!is.null(lab_qc)) {
    req <- c("sample_type", "qc_group", "analyte", "analytical_method", "result", "resultflag",
             "units", "collect_date", "run_date", "lab_name")
    miss <- setdiff(req, names(lab_qc))
    add("lab_qc missing required columns", length(miss) > 0, miss, data = lab_qc)
    if (!length(miss)) {
      add("lab_qc qc_group not accuracy, calibration, blank, other or surrogate",
          !lab_qc$qc_group %in% c("accuracy", "calibration", "blank", "other", "surrogate"),
          lab_qc$qc_group, data = lab_qc)
      units_check(lab_qc$units, lab_qc, "lab_qc: ")
    }
  }

  .finish_contract(issues, stop_on_error)
}

.finish_contract <- function(issues, stop_on_error) {
  out <- if (length(issues)) dplyr::bind_rows(issues) else
    tibble::tibble(check = character(), severity = character(), n_rows = integer(),
                   labs = character(), examples = character())
  warns <- out[out$severity == "warning", ]
  if (nrow(warns)) {
    message("dat contract warnings:
", paste0("- ", warns$check, " (", warns$n_rows, " row(s); e.g. ",
                                                warns$examples, ")", collapse = "
"))
  }
  errs <- out[out$severity == "error", ]
  if (nrow(errs)) {
    msg <- paste0("- ", errs$check, " (", errs$n_rows, " row(s)",
                  ifelse(is.na(errs$labs), "", paste0("; ", errs$labs)),
                  ifelse(is.na(errs$examples), "", paste0("; e.g. ", errs$examples)), ")",
                  collapse = "\n")
    if (stop_on_error) stop("dat contract violations:\n", msg, call. = FALSE)
    message("dat contract violations:\n", msg)
  }
  invisible(out)
}
