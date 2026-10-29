# clock_utils.R
#
# Shared time parsers used by the ingest_*.R functions.

# Clock values arrive either as an Excel fraction of a day (0.559 = 13:25) or as an
# HHMM integer (1325). The format is detected per column; anything else stops.
parse_clock <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ok <- v[!is.na(v)]
  if (!length(ok)) return(hms::as_hms(rep(NA_real_, length(x))))
  if (all(ok >= 0 & ok < 1)) {
    hms::as_hms(round(v * 86400))
  } else if (all(ok >= 0 & ok <= 2359 & (ok %% 100) < 60)) {
    hms::as_hms(floor(v / 100) * 3600 + (v %% 100) * 60)
  } else {
    stop("Unrecognized clock format in values: ",
         paste(utils::head(unique(x[!is.na(v)]), 5), collapse = ", "), call. = FALSE)
  }
}

# "13:31" or "13:31:00" from config.yml
parse_clock_text <- function(x) {
  x <- as.character(x)
  if (grepl("^\\d{1,2}:\\d{2}$", x)) x <- paste0(x, ":00")
  hms::as_hms(x)
}
