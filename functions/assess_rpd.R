# assess_rpd(): field duplicate RPD, evaluated under two guidances side by side.
#
# QAPP v3 (the rule this project operates under): RPD is used only when one or
#   both results are above the LOQ and one or both are at least 2x the LOQ.
#   Otherwise the pair is Not evaluable.
# ADEC 2026 guidance: if either result is below the LOQ, RPD is not performed
#   when the other result is under 2x the LOQ; the pair fails when the other
#   result is at least 2x the LOQ. If both results are at or above the LOQ,
#   RPD is evaluated. Both below the LOQ is not addressed: Not evaluable.
#
# Thresholds (% RPD) are carried over from the legacy Q19 code (nitrate/nitrite
# and phosphorus 25, suspended solids 5, otherwise 20); confirm against the QAPP
# QC table. Non-detects arrive with Result Value 0 and are treated as below LOQ.
#
# Input: a `dat`-style table (WQX column names). Returns one row per
# original/replicate pair.

# RPD limit (%) by characteristic; also the tolerance used by assess_dissolved_total().
# Fecal coliform is NA: QAPP v3 Table 5 gives it no precision criterion (decided 2026-11-24),
# so its pairs are reported with status "No criterion" and never reject.
rpd_threshold <- function(characteristic_name) {
  ifelse(grepl("coliform", characteristic_name, ignore.case = TRUE), NA_real_,
  ifelse(grepl("Nitrate|Nitrite|Phosphorus", characteristic_name, ignore.case = TRUE), 25,
  ifelse(grepl("suspended|solids", characteristic_name, ignore.case = TRUE), 5, 20)))
}

assess_rpd <- function(dat,
                       original_type  = "Field Msr/Obs",
                       replicate_type = "Quality Control Field Replicate Msr/Obs") {
  stopifnot(requireNamespace("dplyr", quietly = TRUE))
  key <- c("Monitoring Location ID", "Activity Start Date", "Characteristic Name",
           "Result Sample Fraction", "Result Analytical Method ID", "Result Unit")

  prep <- function(type, suffix) {
    out <- dat[dat[["Activity Type"]] == type, c(key, "Result Value",
                                                 "Result Detection Limit Value 1")]
    out[["Result Value"]] <- suppressWarnings(as.numeric(out[["Result Value"]]))
    out[["Result Detection Limit Value 1"]] <-
      suppressWarnings(as.numeric(out[["Result Detection Limit Value 1"]]))
    out <- out[!duplicated(out[key]), ]
    names(out)[names(out) == "Result Value"] <- paste0("result_", suffix)
    names(out)[names(out) == "Result Detection Limit Value 1"] <- paste0("loq_", suffix)
    out
  }

  pairs <- merge(prep(original_type, "orig"), prep(replicate_type, "rep"),
                 by = key, all = FALSE)
  names(pairs)[seq_along(key)] <-
    c("monitoring_location_id", "activity_start_date", "characteristic_name",
      "sample_fraction", "method_id", "unit")

  pairs$loq <- pmax(pairs$loq_orig, pairs$loq_rep, na.rm = TRUE)
  pairs$loq[is.infinite(pairs$loq)] <- NA_real_
  pairs$rpd_pct <- abs(pairs$result_orig - pairs$result_rep) /
    ((pairs$result_orig + pairs$result_rep) / 2) * 100
  pairs$threshold <- rpd_threshold(pairs$characteristic_name)

  lo <- pmin(pairs$result_orig, pairs$result_rep)
  hi <- pairs$result_orig |> pmax(pairs$result_rep)
  usable <- !is.na(lo) & !is.na(pairs$loq)
  rpd_ok <- !is.na(pairs$rpd_pct) & is.finite(pairs$rpd_pct)
  no_crit <- is.na(pairs$threshold)
  judge <- ifelse(rpd_ok & pairs$rpd_pct <= pairs$threshold, "Pass", "Fail")

  # QAPP v3
  q_elig <- usable & hi > pairs$loq & hi >= 2 * pairs$loq
  pairs$qapp_v3 <- ifelse(q_elig & rpd_ok, judge, "Not evaluable")
  pairs$qapp_v3[no_crit] <- "No criterion"

  # ADEC 2026
  both_ge <- usable & lo >= pairs$loq
  one_below <- usable & lo < pairs$loq
  pairs$adec_2026 <- ifelse(both_ge & rpd_ok, judge,
                     ifelse(one_below & hi >= 2 * pairs$loq, "Fail", "Not evaluable"))

  pairs$adec_2026[no_crit] <- "No criterion"

  # A QAPP v3 fail with one result below the LOQ (a non-detect or an estimated value)
  # needs a user decision before it can reject (see check_rpd_decisions()).
  pairs$needs_decision <- pairs$qapp_v3 %in% "Fail" & usable & lo < pairs$loq

  pairs$rules_agree <- pairs$qapp_v3 == pairs$adec_2026
  pairs[order(pairs$monitoring_location_id, pairs$activity_start_date,
              pairs$characteristic_name), ]
}

# check_rpd_decisions(): stops unless every pair with needs_decision has a row in the flag
# decisions file (same location, date and characteristic_name; result_sample_fraction blank or
# equal to the pair's fraction) with flag Y (reject both results) or N (accept). Unlike
# automatic Q19 fails, these pairs are not rejected unless the decision is Y.
check_rpd_decisions <- function(rpd_pairs, manual_flags, stop_on_missing = TRUE) {
  need <- rpd_pairs[rpd_pairs$needs_decision %in% TRUE, , drop = FALSE]
  if (nrow(need) == 0) return(invisible(need))
  decided <- rep(FALSE, nrow(need))
  if (!is.null(manual_flags) && nrow(manual_flags) > 0) {
    mf <- manual_flags[toupper(trimws(manual_flags$flag)) %in% c("Y", "N"), , drop = FALSE]
    frac <- if ("result_sample_fraction" %in% names(mf)) mf$result_sample_fraction else
      rep(NA_character_, nrow(mf))
    frac[!is.na(frac) & trimws(frac) == ""] <- NA_character_
    for (i in seq_len(nrow(need))) {
      decided[i] <- any(
        mf$monitoring_location_id == need$monitoring_location_id[i] &
          as.character(as.Date(mf$activity_start_date)) ==
          as.character(as.Date(need$activity_start_date[i])) &
          mf$characteristic_name == need$characteristic_name[i] &
          (is.na(frac) | frac %in% need$sample_fraction[i]))
    }
  }
  undecided <- need[!decided, , drop = FALSE]
  if (nrow(undecided) > 0 && stop_on_missing) {
    lines <- sprintf("  %s, %s, %s (%s): original %g, replicate %g, LOQ %g, RPD %.1f%%",
                     undecided$monitoring_location_id,
                     format(as.Date(undecided$activity_start_date), "%m/%d/%Y"),
                     undecided$characteristic_name, undecided$sample_fraction,
                     undecided$result_orig, undecided$result_rep, undecided$loq,
                     undecided$rpd_pct)
    stop("Q19: field duplicate pair fails the RPD test with one result below the LOQ and no ",
         "decision is recorded for:
", paste(lines, collapse = "
"),
         "
Add a row for each to the flag decisions file (columns activity_start_date ",
         "[M/D/YYYY], characteristic_name, monitoring_location_id, result_sample_fraction, ",
         "flag, notes). flag = Y rejects both results; flag = N accepts them; explain in notes.",
         call. = FALSE)
  }
  invisible(undecided)
}
