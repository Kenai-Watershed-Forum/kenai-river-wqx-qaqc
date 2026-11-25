# check_config.R
#
# Pre-flight check of config.yml for a new project-year repo. Stops with one message
# listing every problem, so a fresh copy of the template fails with a clear error
# instead of an obscure ingest failure.
#
# Arguments
#   config    list read from config.yml
#   data_dir  folder holding the year's raw lab files
#
# Checks
#   1. year is a four-digit number
#   2. spring_date and summer_date parse as dates, fall in `year`, spring before summer
#   3. every lab file entry (sgs, fc, tss: spring and summer; ysi: both) has a non-blank
#      path to a file that exists under data_dir, and a non-blank sheet where a sheet is used
#   4. tss run_date parses as a date and run_time is non-blank (both required by ingest_tss)
#
# Returns TRUE invisibly when all checks pass.

check_config <- function(config, data_dir = here::here("data")) {
  problems <- character()
  add <- function(...) problems <<- c(problems, paste0(...))
  blank <- function(x) is.null(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(as.character(x)))
  as_date <- function(x) if (blank(x)) as.Date(NA) else suppressWarnings(as.Date(as.character(x), format = "%Y-%m-%d"))

  yr <- suppressWarnings(as.integer(config$year))
  if (blank(config$year) || is.na(yr) || yr < 1900 || yr > 2200) {
    add("year is blank or not a four-digit year")
    yr <- NA_integer_
  }

  dates <- list(spring_date = as_date(config$sampling$spring_date),
                summer_date = as_date(config$sampling$summer_date))
  for (nm in names(dates)) {
    d <- dates[[nm]]
    if (is.na(d)) {
      add("sampling$", nm, " is blank or not a YYYY-MM-DD date")
    } else if (!is.na(yr) && as.integer(format(d, "%Y")) != yr) {
      add("sampling$", nm, " (", d, ") is not in year ", yr)
    }
  }
  if (!anyNA(dates) && dates$spring_date >= dates$summer_date) {
    add("sampling$spring_date must be before summer_date")
  }

  entries <- list(sgs = c("spring", "summer"), fc = c("spring", "summer"),
                  tss = c("spring", "summer"), ysi = "both")
  needs_sheet <- c(sgs = TRUE, fc = FALSE, tss = TRUE, ysi = FALSE)
  for (lab in names(entries)) {
    for (s in entries[[lab]]) {
      spec <- config$files[[lab]][[s]]
      id <- paste0("files$", lab, "$", s)
      if (is.null(spec)) { add(id, " is missing"); next }
      if (blank(spec$path)) {
        add(id, "$path is blank")
      } else if (!file.exists(file.path(data_dir, spec$path))) {
        add(id, "$path not found in ", data_dir, ": ", spec$path)
      }
      if (needs_sheet[[lab]] && blank(spec$sheet)) add(id, "$sheet is blank")
      if (lab == "tss") {
        if (is.na(as_date(spec$run_date))) add(id, "$run_date is blank or not a YYYY-MM-DD date")
        if (blank(spec$run_time)) add(id, "$run_time is blank")
      }
    }
  }

  if (length(problems)) {
    stop("config.yml is not ready:\n  - ", paste(problems, collapse = "\n  - "), call. = FALSE)
  }
  invisible(TRUE)
}
