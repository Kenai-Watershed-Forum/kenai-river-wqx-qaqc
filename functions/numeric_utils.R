# numeric_utils.R
#
# Shared strict numeric conversion used by the ingest_*.R functions.
#
# Why: as.numeric() on text turns an entry error such as "O.18" (letter O for zero) into NA, and
# the ingest code used to hide the warning. In 2021 a detection limit stored as "O.18" reached
# WQP. A silent NA can also pass the contract check (a U flag allows an NA result).

# to_numeric_strict(x, what, labels = NULL)
#   x       vector; numeric input is returned unchanged
#   what    description used in the error message (file and column)
#   labels  optional vector, same length as x, naming each row (e.g. sample or site)
# Blank, NA and whitespace-only values stay NA. Any other value that is not a number stops,
# listing the raw text, the row labels, and a hint when swapping lookalike characters
# (O or o for 0, l or I for 1) would make it numeric.
to_numeric_strict <- function(x, what, labels = NULL) {
  if (is.numeric(x)) return(x)
  txt <- trimws(as.character(x))
  blank <- is.na(txt) | txt == ""
  v <- suppressWarnings(as.numeric(txt))
  bad <- !blank & is.na(v)
  if (!any(bad)) return(v)

  vals <- unique(txt[bad])
  fixed <- chartr("OoIl", "0011", vals)
  fixed_ok <- !is.na(suppressWarnings(as.numeric(fixed))) & fixed != vals
  hint <- ifelse(fixed_ok, paste0(" (did you mean ", fixed, "?)"), "")
  shown <- utils::head(paste0("'", vals, "'", hint), 10)

  where <- ""
  if (!is.null(labels)) {
    lab <- unique(as.character(labels[bad]))
    where <- paste0("; rows: ", paste(utils::head(lab, 10), collapse = ", "))
  }
  stop("Non-numeric text in ", what, ": ", paste(shown, collapse = ", "),
       if (length(vals) > 10) paste0(" and ", length(vals) - 10, " more") else "",
       where, ". Fix the source file.", call. = FALSE)
}
