# check_cdx_fractions(): stop before the CDX files are written if any result row has a missing
# or invalid Result Sample Fraction.
#
# format_wqx.R fills the fraction by left join, so a method missing from the lookup sheet gets NA
# without an error. WQX Web rejects a missing or wrong fraction on import (2021 lesson), so this
# is the last place to catch it.
#
# Args:
#   results   the export data frame (columns `Result Sample Fraction`, `Characteristic Name`,
#             `Result Analytical Method ID`)
#   fractions character vector of allowed values, from
#             reference/wqx_templates/wqx_sample_fraction_codes.csv
#
# Returns `results` invisibly when valid; otherwise stops listing the offending combinations.

check_cdx_fractions <- function(results, fractions) {
  needed <- c("Result Sample Fraction", "Characteristic Name", "Result Analytical Method ID")
  missing_cols <- setdiff(needed, names(results))
  if (length(missing_cols) > 0) {
    stop("check_cdx_fractions: results is missing column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (length(fractions) == 0 || anyNA(fractions)) {
    stop("check_cdx_fractions: the allowed fraction list is empty or has NA", call. = FALSE)
  }

  frac <- results[["Result Sample Fraction"]]
  blank <- is.na(frac) | trimws(frac) == ""
  invalid <- !blank & !frac %in% fractions
  bad <- blank | invalid

  if (any(bad)) {
    summary_tbl <- data.frame(
      issue = ifelse(blank[bad], "missing", "not in WQX list"),
      fraction = ifelse(blank[bad], "", frac[bad]),
      characteristic = results[["Characteristic Name"]][bad],
      method = results[["Result Analytical Method ID"]][bad],
      stringsAsFactors = FALSE
    )
    counts <- stats::aggregate(
      list(n_rows = rep(1L, nrow(summary_tbl))), summary_tbl, FUN = sum
    )
    lines <- paste0("  ", counts$issue, " | '", counts$fraction, "' | ", counts$characteristic,
                    " | method ", counts$method, " | ", counts$n_rows, " row(s)")
    stop("check_cdx_fractions: ", sum(bad), " row(s) have a missing or invalid Result Sample ",
         "Fraction. Add the method to the result_sample_fraction sheet of ",
         "wqx_template_matching_table.xlsx or fix the Part A value:\n",
         paste(lines, collapse = "\n"), call. = FALSE)
  }
  invisible(results)
}
