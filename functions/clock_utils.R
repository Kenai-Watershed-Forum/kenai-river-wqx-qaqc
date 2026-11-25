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

# Alaska time zone abbreviation ("AKDT" or "AKST") for a date. All sampling is in the Kenai
# watershed, so the zone follows from the date alone (daylight time runs from the second Sunday
# of March to the first Sunday of November); the tz database supplies the rules. Noon is used so
# the transition hour (2 a.m.) cannot matter. NA dates return NA.
akt_zone <- function(date) {
  d <- as.Date(date)
  out <- rep(NA_character_, length(d))
  ok <- !is.na(d)
  out[ok] <- format(as.POSIXct(paste(d[ok], "12:00:00"), tz = "America/Anchorage"), "%Z")
  out
}
