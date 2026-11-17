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
  pairs$threshold <- ifelse(grepl("Nitrate|Nitrite|Phosphorus", pairs$characteristic_name,
                                  ignore.case = TRUE), 25,
                     ifelse(grepl("suspended|solids", pairs$characteristic_name,
                                  ignore.case = TRUE), 5, 20))

  lo <- pmin(pairs$result_orig, pairs$result_rep)
  hi <- pairs$result_orig |> pmax(pairs$result_rep)
  usable <- !is.na(lo) & !is.na(pairs$loq)
  rpd_ok <- !is.na(pairs$rpd_pct) & is.finite(pairs$rpd_pct)
  judge <- ifelse(rpd_ok & pairs$rpd_pct <= pairs$threshold, "Pass", "Fail")

  # QAPP v3
  q_elig <- usable & hi > pairs$loq & hi >= 2 * pairs$loq
  pairs$qapp_v3 <- ifelse(q_elig & rpd_ok, judge, "Not evaluable")

  # ADEC 2026
  both_ge <- usable & lo >= pairs$loq
  one_below <- usable & lo < pairs$loq
  pairs$adec_2026 <- ifelse(both_ge & rpd_ok, judge,
                     ifelse(one_below & hi >= 2 * pairs$loq, "Fail", "Not evaluable"))

  pairs$rules_agree <- pairs$qapp_v3 == pairs$adec_2026
  pairs[order(pairs$monitoring_location_id, pairs$activity_start_date,
              pairs$characteristic_name), ]
}
