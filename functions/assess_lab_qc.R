# assess_lab_qc.R
#
# Q20 lab QC assessment. Adds `qc_check` and `qc_result` to the lab_qc table.
# Limits come from the lab's own columns; nothing is hardcoded.
#
# Checks (a check applies when its data column is present on the row)
#   Recovery  percent_recovered vs rec_limit_low/high (inclusive); covers (surr) analytes
#   RPD       sample_rpd vs rpd_limit_high
#   Blank     qc_group "blank": QAPP rule (primary, drives qc_result): Fail on any detection (flag not U/ND;
#             if the flag is missing, result above dl). ADEC rule (blank_adec, shown beside it): Fail at or above loq.
# A check whose limit is missing is not evaluable. A row is Fail if any check fails,
# Pass if at least one check passes and none fail, otherwise Not evaluable (including
# rows with no applicable check). A missing limit never counts as a pass.
#
# OPEN TASK: SGS gives LLQC rows no recovery limits, so they are Not evaluable. Ask SGS
# whether they apply LLQC acceptance limits (see topics/known_issues.md).
#
# Arguments
#   lab_qc  data frame with columns qc_group, result, resultflag,
#           percent_recovered, rec_limit_low, rec_limit_high, sample_rpd, rpd_limit_high, loq, dl
# Returns lab_qc with qc_check (checks applied, "None" if none), qc_result, and blank_qapp and
# blank_adec (NA on non-blank rows).
assess_lab_qc <- function(lab_qc) {
  need <- c("qc_group", "result", "resultflag", "percent_recovered",
            "rec_limit_low", "rec_limit_high", "sample_rpd", "rpd_limit_high", "loq", "dl")
  missing <- setdiff(need, names(lab_qc))
  if (length(missing) > 0) {
    stop("assess_lab_qc: lab_qc is missing column(s): ", paste(missing, collapse = ", "))
  }

  # One outcome per check: NA (check not applicable), "Pass", "Fail" or "Not evaluable"
  rec <- ifelse(is.na(lab_qc$percent_recovered), NA_character_,
         ifelse(is.na(lab_qc$rec_limit_low) | is.na(lab_qc$rec_limit_high), "Not evaluable",
         ifelse(lab_qc$percent_recovered < lab_qc$rec_limit_low |
                  lab_qc$percent_recovered > lab_qc$rec_limit_high, "Fail", "Pass")))

  rpd <- ifelse(is.na(lab_qc$sample_rpd), NA_character_,
         ifelse(is.na(lab_qc$rpd_limit_high), "Not evaluable",
         ifelse(lab_qc$sample_rpd > lab_qc$rpd_limit_high, "Fail", "Pass")))

  is_blank <- lab_qc$qc_group %in% "blank"
  nd <- lab_qc$resultflag %in% c("U", "ND")
  has_flag <- !is.na(lab_qc$resultflag)
  blk_qapp <- ifelse(!is_blank, NA_character_,
              ifelse(nd, "Pass",
              ifelse(is.na(lab_qc$result), "Not evaluable",
              ifelse(has_flag, "Fail",
              ifelse(is.na(lab_qc$dl), "Not evaluable",
              ifelse(lab_qc$result > lab_qc$dl, "Fail", "Pass"))))))

  blk_adec <- ifelse(!is_blank, NA_character_,
              ifelse(nd, "Pass",
              ifelse(is.na(lab_qc$result) | is.na(lab_qc$loq), "Not evaluable",
              ifelse(lab_qc$result >= lab_qc$loq, "Fail", "Pass"))))

  # Only the QAPP outcome enters qc_result; the ADEC outcome is for comparison
  blk <- blk_qapp

  outcomes <- cbind(Recovery = rec, RPD = rpd, Blank = blk)
  applied <- apply(outcomes, 1, function(x) {
    a <- names(x)[!is.na(x)]
    if (length(a) == 0) "None" else paste(a, collapse = "+")
  })
  result <- apply(outcomes, 1, function(x) {
    x <- x[!is.na(x)]
    if (any(x == "Fail")) "Fail"
    else if (any(x == "Pass")) "Pass"
    else "Not evaluable"
  })

  lab_qc$qc_check <- unname(applied)
  lab_qc$qc_result <- unname(result)
  lab_qc$blank_qapp <- blk_qapp
  lab_qc$blank_adec <- blk_adec
  lab_qc
}
