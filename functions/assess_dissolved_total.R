# assess_dissolved_total(): Q18, dissolved vs total (unfiltered) results.
#
# For every characteristic that has both a Dissolved and a total result for the same
# location and date, compares the two (primary samples only; duplicates and blanks are
# excluded). "Total" means Result Sample Fraction Total or Unfiltered: SGS's EDD marks a
# filtered metals sample "L" (ingested as Dissolved) and an unfiltered one "." (ingested as
# Unfiltered).
#
# Status per pair:
#   OK                                       dissolved <= total
#   Dissolved > total (within tolerance)     dissolved > total, RPD <= the RPD limit
#                                            (rpd_threshold() in assess_rpd.R)
#   Dissolved > total (exceeds tolerance)    dissolved > total, RPD > the RPD limit
#   Not comparable                           either result is a non-detect or below the
#                                            quantification limit (Result Detection
#                                            Condition is set), or a value is missing
# Only "exceeds tolerance" pairs need a user decision (see
# check_dissolved_total_decisions()). Nothing is rejected here.
#
# Input: the WQX intermediate table with snake_case names (as in apply_qaqc_flags.R):
#   monitoring_location_id, activity_start_date, activity_type, characteristic_name,
#   result_sample_fraction, result_value, result_unit, result_detection_condition, dat_row.
# Units are converted to ug/L; mg/L, ug/L and ng/L are recognized, anything else stops.

assess_dissolved_total <- function(export_dat, primary_type = "Field Msr/Obs",
                                   total_fractions = c("Total", "Unfiltered")) {
  need <- c("monitoring_location_id", "activity_start_date", "activity_type",
            "characteristic_name", "result_sample_fraction", "result_value", "result_unit",
            "result_detection_condition", "dat_row")
  miss <- setdiff(need, names(export_dat))
  if (length(miss) > 0) {
    stop("assess_dissolved_total: export_dat is missing column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }

  d <- export_dat[export_dat$activity_type %in% primary_type &
                    export_dat$result_sample_fraction %in% c("Dissolved", total_fractions), ,
                  drop = FALSE]
  empty <- data.frame(
    monitoring_location_id = character(), activity_start_date = as.Date(character()),
    characteristic_name = character(), unit = character(), dissolved_result = numeric(),
    total_result = numeric(), total_fraction = character(), rpd_pct = numeric(),
    threshold = numeric(), dt_status = character(), dissolved_dat_row = integer(),
    total_dat_row = integer(), stringsAsFactors = FALSE)
  # Unit conversion only matters for characteristics measured in both fractions
  has_both <- with(d, tapply(result_sample_fraction == "Dissolved", characteristic_name, any) &
                     tapply(result_sample_fraction != "Dissolved", characteristic_name, any))
  d <- d[d$characteristic_name %in% names(has_both)[has_both], , drop = FALSE]
  if (nrow(d) == 0) return(empty)

  to_ugl <- c(`ug/L` = 1, `mg/L` = 1000, `ng/L` = 0.001)
  bad_unit <- setdiff(unique(d$result_unit), names(to_ugl))
  if (length(bad_unit) > 0) {
    stop("assess_dissolved_total: unrecognized unit(s): ", paste(bad_unit, collapse = ", "),
         call. = FALSE)
  }
  d$value <- suppressWarnings(as.numeric(d$result_value)) * to_ugl[d$result_unit]
  d$is_total <- d$result_sample_fraction %in% total_fractions
  d$not_quantified <- !is.na(d$result_detection_condition) & d$result_detection_condition != ""
  d$date <- as.Date(d$activity_start_date)

  key <- paste(d$monitoring_location_id, d$date, d$characteristic_name, sep = "|")
  side <- ifelse(d$is_total, "total", "dissolved")
  dup <- duplicated(paste(key, side))
  if (any(dup)) {
    stop("assess_dissolved_total: more than one ", "primary result per fraction for: ",
         paste(unique(key[dup]), collapse = "; "), call. = FALSE)
  }

  diss <- d[!d$is_total, ]
  tot <- d[d$is_total, ]
  m <- merge(
    data.frame(key = key[!d$is_total], diss_val = diss$value, diss_nq = diss$not_quantified,
               diss_row = diss$dat_row, monitoring_location_id = diss$monitoring_location_id,
               activity_start_date = diss$date, characteristic_name = diss$characteristic_name,
               stringsAsFactors = FALSE),
    data.frame(key = key[d$is_total], tot_val = tot$value, tot_nq = tot$not_quantified,
               tot_row = tot$dat_row, total_fraction = tot$result_sample_fraction,
               stringsAsFactors = FALSE),
    by = "key")
  if (nrow(m) == 0) return(empty)

  m$unit <- "ug/L"
  m$rpd_pct <- abs(m$diss_val - m$tot_val) / ((m$diss_val + m$tot_val) / 2) * 100
  m$threshold <- rpd_threshold(m$characteristic_name)
  comparable <- !is.na(m$diss_val) & !is.na(m$tot_val) & !m$diss_nq & !m$tot_nq
  m$dt_status <- ifelse(!comparable, "Not comparable",
                 ifelse(m$diss_val <= m$tot_val, "OK",
                 ifelse(!is.na(m$rpd_pct) & m$rpd_pct <= m$threshold,
                        "Dissolved > total (within tolerance)",
                        "Dissolved > total (exceeds tolerance)")))

  out <- data.frame(
    monitoring_location_id = m$monitoring_location_id,
    activity_start_date = m$activity_start_date,
    characteristic_name = m$characteristic_name, unit = m$unit,
    dissolved_result = m$diss_val, total_result = m$tot_val,
    total_fraction = m$total_fraction, rpd_pct = m$rpd_pct, threshold = m$threshold,
    dt_status = m$dt_status, dissolved_dat_row = m$diss_row, total_dat_row = m$tot_row,
    stringsAsFactors = FALSE)
  out[order(out$monitoring_location_id, out$activity_start_date, out$characteristic_name), ]
}

# check_dissolved_total_decisions(): stops unless every "exceeds tolerance" pair has a
# decision in the flag decisions file. A decision is a row matching the pair's location,
# date and characteristic_name (result_sample_fraction blank, Dissolved or a total
# fraction) with flag "Y" (reject) or "N" (accept, explain in notes). A flag "Y" row with
# a blank fraction rejects both results; with a fraction it rejects only that result.
# Returns the undecided rows invisibly when stop_on_missing = FALSE.

check_dissolved_total_decisions <- function(pairs, manual_flags, stop_on_missing = TRUE) {
  need <- pairs[pairs$dt_status %in% "Dissolved > total (exceeds tolerance)", , drop = FALSE]
  if (nrow(need) == 0) return(invisible(need))

  decided <- rep(FALSE, nrow(need))
  if (!is.null(manual_flags) && nrow(manual_flags) > 0) {
    mf <- manual_flags[toupper(trimws(manual_flags$flag)) %in% c("Y", "N"), , drop = FALSE]
    if (nrow(mf) > 0) {
      mk <- paste(mf$monitoring_location_id, as.character(as.Date(mf$activity_start_date)),
                  mf$characteristic_name, sep = "|")
      nk <- paste(need$monitoring_location_id, as.character(need$activity_start_date),
                  need$characteristic_name, sep = "|")
      decided <- nk %in% mk
    }
  }
  undecided <- need[!decided, , drop = FALSE]

  if (nrow(undecided) > 0 && stop_on_missing) {
    lines <- sprintf("  %s, %s, %s: dissolved %.3g > total %.3g ug/L (RPD %.1f%%, limit %g%%)",
                     undecided$monitoring_location_id,
                     format(undecided$activity_start_date, "%m/%d/%Y"),
                     undecided$characteristic_name, undecided$dissolved_result,
                     undecided$total_result, undecided$rpd_pct, undecided$threshold)
    stop("Q18: dissolved exceeds total beyond tolerance and no decision is recorded for:\n",
         paste(lines, collapse = "\n"),
         "\nAdd a row for each to the flag decisions file (columns activity_start_date ",
         "[M/D/YYYY], characteristic_name, monitoring_location_id, result_sample_fraction, ",
         "flag, notes). flag = Y rejects (leave result_sample_fraction blank for both ",
         "results, or give Dissolved or Unfiltered/Total for one); flag = N accepts; ",
         "explain either in notes.", call. = FALSE)
  }
  invisible(undecided)
}
