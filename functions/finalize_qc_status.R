# finalize_qc_status.R
#
# Adds the two QC sources that can only be applied to the WQX-formatted export to the
# lab-QC status that assign_qc_status() already carried through format_wqx.R:
#   1. Q19 field duplicate failures (assess_rpd(), QAPP v3 rule): both the original and
#      the replicate result are Rejected.
#   2. The manual decisions file (rows with flag "Y"), matched on monitoring location,
#      activity start date and characteristic name.
# A result is Rejected if any source rejects it; reasons are joined with "; ".
#
# Arguments
#   export_dat    the intermediate export with snake_case names (janitor::clean_names()), including
#                 qc_status, qc_fail_reason, activity_type, monitoring_location_id,
#                 activity_start_date, characteristic_name, result_sample_fraction,
#                 result_analytical_method_id, result_unit
#   rpd_pairs     assess_rpd() output, or NULL
#   manual_flags  data frame with monitoring_location_id, activity_start_date, characteristic_name,
#                 flag, notes (optional column), result_sample_fraction (optional; blank = all
#                 fractions), or NULL. Unmatched "Y" rows give a warning.
#   holding_times assess_holding_times() output, or NULL. Rows with hold_status "Fail" are Rejected
#                 (matched on dat_row, which export_dat must carry) with hold_note as the reason.
# Returns export_dat with qc_status, qc_fail_reason and flag ("Y" when Rejected, else "N").

finalize_qc_status <- function(export_dat, rpd_pairs = NULL, manual_flags = NULL,
                               holding_times = NULL,
                               original_type = "Field Msr/Obs",
                               replicate_type = "Quality Control Field Replicate Msr/Obs") {
  need <- c("qc_status", "qc_fail_reason", "activity_type", "monitoring_location_id",
            "activity_start_date", "characteristic_name", "result_sample_fraction",
            "result_analytical_method_id", "result_unit")
  miss <- setdiff(need, names(export_dat))
  if (length(miss) > 0) {
    stop("finalize_qc_status: export_dat is missing column(s): ", paste(miss, collapse = ", "),
         call. = FALSE)
  }

  d <- export_dat
  key <- function(...) paste(..., sep = "|")
  add_reason <- function(existing, idx, text) {
    existing[idx] <- ifelse(is.na(existing[idx]), text, paste(existing[idx], text, sep = "; "))
    existing
  }
  reason <- d$qc_fail_reason
  reason[!is.na(reason) & reason == ""] <- NA_character_
  rejected <- d$qc_status %in% "Rejected"

  # Q19: both rows of a failing pair
  if (!is.null(rpd_pairs) && nrow(rpd_pairs) > 0) {
    # Pairs needing a user decision are rejected only through a "Y" manual row
    auto <- if ("needs_decision" %in% names(rpd_pairs)) !(rpd_pairs$needs_decision %in% TRUE) else
      rep(TRUE, nrow(rpd_pairs))
    fails <- rpd_pairs[rpd_pairs$qapp_v3 %in% "Fail" & auto, , drop = FALSE]
    if (nrow(fails) > 0) {
      pk <- key(fails$monitoring_location_id, as.character(fails$activity_start_date),
                fails$characteristic_name, fails$sample_fraction, fails$method_id, fails$unit)
      ek <- key(d$monitoring_location_id, as.character(d$activity_start_date),
                d$characteristic_name, d$result_sample_fraction, d$result_analytical_method_id,
                d$result_unit)
      hit <- match(ek, pk)
      idx <- which(d$activity_type %in% c(original_type, replicate_type) & !is.na(hit))
      reason <- add_reason(reason, idx,
        sprintf("Field duplicate RPD fail (QAPP v3): %.1f%% exceeds %g%%",
                fails$rpd_pct[hit[idx]], fails$threshold[hit[idx]]))
      rejected[idx] <- TRUE
    }
  }

  # Q17: hold time failures, matched on dat_row
  if (!is.null(holding_times) && nrow(holding_times) > 0) {
    if (!"dat_row" %in% names(d)) {
      stop("finalize_qc_status: export_dat needs dat_row to apply holding_times", call. = FALSE)
    }
    hf <- holding_times[holding_times$hold_status %in% "Fail", , drop = FALSE]
    idx <- which(d$dat_row %in% hf$dat_row)
    if (length(idx) > 0) {
      reason <- add_reason(reason, idx, hf$hold_note[match(d$dat_row[idx], hf$dat_row)])
      rejected[idx] <- TRUE
    }
  }

  # Manual decisions
  if (!is.null(manual_flags) && nrow(manual_flags) > 0) {
    mf <- manual_flags[manual_flags$flag %in% "Y", , drop = FALSE]
    if (nrow(mf) > 0) {
      notes <- if ("notes" %in% names(mf)) mf$notes else rep(NA_character_, nrow(mf))
      # Optional result_sample_fraction: blank or absent matches every fraction
      frac <- if ("result_sample_fraction" %in% names(mf)) mf$result_sample_fraction else
        rep(NA_character_, nrow(mf))
      frac[!is.na(frac) & trimws(frac) == ""] <- NA_character_
      mk <- key(mf$monitoring_location_id, as.character(as.Date(mf$activity_start_date)),
                mf$characteristic_name)
      ek <- key(d$monitoring_location_id, as.character(as.Date(d$activity_start_date)),
                d$characteristic_name)
      hits <- lapply(seq_along(mk), function(i) {
        which(ek == mk[i] & (is.na(frac[i]) | d$result_sample_fraction %in% frac[i]))
      })
      unmatched <- lengths(hits) == 0
      if (any(unmatched)) {
        warning("finalize_qc_status: manual flag row(s) matched no result: ",
                paste(ifelse(is.na(frac[unmatched]), mk[unmatched],
                             paste(mk[unmatched], frac[unmatched], sep = "|")),
                      collapse = "; "), call. = FALSE)
      }
      text <- ifelse(is.na(notes) | notes == "", "Manual flag",
                     paste0("Manual flag: ", notes))
      for (i in seq_along(mk)) {
        idx <- hits[[i]]
        if (length(idx) > 0) {
          reason <- add_reason(reason, idx, text[i])
          rejected[idx] <- TRUE
        }
      }
    }
  }

  d$qc_fail_reason <- reason
  d$qc_status <- ifelse(rejected, "Rejected", "Accepted")
  d$flag <- ifelse(rejected, "Y", "N")
  d
}
