# Stage 2 of the lab-report PDF extraction: map the Stage 1 output
# (extract_ars_pdf.R) to the 72-column KWF/EPA template, long format.
#
# Values come from the PDF as printed. Project constants and rules live in
# template_constants.yml. Monitoring Location ID, Activity ID, Result Sample
# Fraction, Method Context and other derived columns are left blank on purpose;
# site names are reconciled in a later step.

map_ars_to_template <- function(extract,
                                config_path = "functions/pdf_extract/template_constants.yml",
                                columns_path = "functions/pdf_extract/template_columns.csv") {
  cfg <- yaml::read_yaml(config_path)
  cols <- utils::read.csv(columns_path, stringsAsFactors = FALSE)$column
  res <- extract$results

  bad_const <- setdiff(names(cfg$constants), cols)
  if (length(bad_const)) stop("Constants name unknown template columns: ", paste(bad_const, collapse = ", "), call. = FALSE)
  if (!cfg$recovery_qc_type %in% c("LCS", "MS", "MSD")) stop("recovery_qc_type must be LCS, MS or MSD.", call. = FALSE)

  lab_qc <- grepl(cfg$lab_qc_name_pattern, res$client_sample_name, ignore.case = TRUE)
  if (any(lab_qc)) {
    stop("Lab QC sample names in results: ", paste(unique(res$client_sample_name[lab_qc]), collapse = ", "), call. = FALSE)
  }

  # Sample classes from the client sample name
  condition <- rep(NA_character_, nrow(res))
  activity <- rep(cfg$default_activity_type, nrow(res))
  done <- rep(FALSE, nrow(res))
  for (rule in cfg$sample_classes) {
    hit <- !done & grepl(rule$pattern, res$client_sample_name, ignore.case = TRUE, perl = TRUE)
    condition[hit] <- rule$sample_condition
    activity[hit] <- rule$activity_type
    done <- done | hit
  }

  # Qualifiers: U for non-detects; J from the lab flag or from MDL <= result < PQL
  has_j <- grepl("\\bJ\\b", res$flags)
  rule_j <- !res$non_detect & !is.na(res$result_value) & !is.na(res$pql) &
    res$result_value < res$pql & (is.na(res$mdl) | res$result_value >= res$mdl)
  qualifier <- ifelse(res$non_detect, cfg$qualifiers$non_detect,
                      ifelse(has_j | rule_j, cfg$qualifiers$below_pql, NA_character_))
  other_flags <- trimws(gsub("\\bJ\\b", "", res$flags))
  other_flags <- ifelse(nzchar(other_flags), other_flags, NA_character_)

  # Recovery, spike and control limits from the Stage 1 QC table
  qc <- extract$qc[extract$qc$qc_type == cfg$recovery_qc_type, ]
  if (cfg$recovery_qc_type == "LCS") {
    keys <- c("prep_batch", "method_id", "analyte")
    res$qc_key <- paste(res$prep_batch, res$method_id, res$analyte, sep = "|")
  } else {
    keys <- c("prep_batch", "method_id", "analyte", "parent_sample")
    res$qc_key <- paste(res$prep_batch, res$method_id, res$analyte, res$lab_sample_number, sep = "|")
  }
  qc <- unique(qc[, c(keys, "recovery", "spike_level", "lcl", "ucl", "flag")])
  qc$qc_key <- do.call(paste, c(unname(as.list(qc[keys])), sep = "|"))
  # The same QC record can print in several reports of one event; recoveries may differ by
  # lab rounding. Tolerate a small recovery difference (keep first); stop on any other conflict.
  rec_tol <- 0.2
  dup_keys <- unique(qc$qc_key[duplicated(qc$qc_key)])
  rounding_log <- NULL
  for (k in dup_keys) {
    g <- qc[qc$qc_key == k, ]
    same_other <- all(vapply(c("spike_level", "lcl", "ucl", "flag"), \(cn) length(unique(g[[cn]])) == 1, TRUE))
    rec_range <- diff(range(g$recovery, na.rm = TRUE))
    if (!same_other || !is.finite(rec_range) || rec_range > rec_tol) {
      stop("Conflicting ", cfg$recovery_qc_type, " records for: ", k, call. = FALSE)
    }
    rounding_log <- rbind(rounding_log, data.frame(qc_key = k, n_reports = nrow(g),
                          recovery_values = paste(g$recovery, collapse = " / "), kept = g$recovery[1]))
  }
  qc <- qc[!duplicated(qc$qc_key), ]
  m <- match(res$qc_key, qc$qc_key)

  # Result Detection Condition from the qualifier (same convention as format_wqx.R)
  detection <- ifelse(qualifier %in% cfg$qualifiers$non_detect, cfg$detection_conditions$non_detect,
                      ifelse(qualifier %in% cfg$qualifiers$below_pql, cfg$detection_conditions$below_pql,
                             NA_character_))

  # Sample receipt: the lab that received the samples (primary), by report
  rec <- extract$sample_receipt[extract$sample_receipt$role == "primary", ]
  r_i <- match(res$source_file, rec$source_file)
  if (anyNA(r_i)) {
    stop("No primary receipt record for: ", paste(unique(res$source_file[is.na(r_i)]), collapse = ", "), call. = FALSE)
  }

  lab_comment <- vapply(seq_len(nrow(res)), \(i) {
    parts <- c(if (!is.na(other_flags[i])) paste("Lab flag:", other_flags[i]),
               if (!is.na(m[i]) && !is.na(qc$flag[m[i]])) paste0(cfg$recovery_qc_type, " flag: ", qc$flag[m[i]]))
    if (length(parts)) paste(parts, collapse = "; ") else NA_character_
  }, "")

  out <- tibble::as_tibble(stats::setNames(rep(list(rep(NA_character_, nrow(res))), length(cols)), cols))
  out[["Client Sample Name"]] <- res$client_sample_name
  out[["Sample Condition"]] <- condition
  out[["Lab Sample Number"]] <- res$lab_sample_base
  out[["Prep Date"]] <- res$prep_date
  out[["Prep Batch Number"]] <- res$prep_batch
  out[["Analyst Initials"]] <- res$analyst
  out[["Activity Start Date"]] <- res$collect_date
  out[["Activity Start Time"]] <- res$collect_time
  out[["Activity End Date"]] <- res$collect_date
  out[["Activity End Time"]] <- res$collect_time
  out[["Activity Type"]] <- activity
  out[["Characteristic Name"]] <- res$analyte
  out[["Result Analytical Method ID"]] <- res$method_id
  out[["Result Value"]] <- res$result_value
  out[["Result Unit"]] <- res$units
  out[["Result Qualifier"]] <- qualifier
  out[["Result Weight Basis"]] <- res$report_basis
  out[["Result Detection Limit Value 1"]] <- res$pql
  out[["Result Detection Limit Unit 1"]] <- res$units
  out[["Result Detection Limit Value 2"]] <- res$mdl
  out[["Result Detection Limit Unit 2"]] <- res$units
  out[["Spike"]] <- qc$spike_level[m]
  out[["Percent Recovered"]] <- qc$recovery[m]
  out[["LCL"]] <- qc$lcl[m]
  out[["UCL"]] <- qc$ucl[m]
  out[["Laboratory Name"]] <- res$performing_lab
  out[["Laboratory Comment Text"]] <- lab_comment
  out[["Analysis Start Date"]] <- res$analysis_date
  out[["Analysis Start Time"]] <- res$analysis_time
  out[["Sample Receipt Date"]] <- rec$receipt_date[r_i]
  out[["Sample Receipt Time"]] <- rec$receipt_time[r_i]
  out[["Result Detection Condition"]] <- detection

  for (nm in names(cfg$constants)) {
    if (nzchar(cfg$constants[[nm]])) out[[nm]] <- cfg$constants[[nm]]
  }
  out <- out[, cols]

  checks <- list(
    sample_classes = unique(data.frame(client_sample_name = res$client_sample_name,
                                       sample_condition = condition, activity_type = activity)),
    qualifier_vs_flag = unique(data.frame(
      source_file = res$source_file, lab_sample = res$lab_sample_base, analyte = res$analyte,
      result = res$result_value, pql = res$pql, mdl = res$mdl, flags = res$flags,
      qualifier = qualifier)[has_j != rule_j & !res$non_detect, ]),
    recovery_coverage = stats::aggregate(
      has_qc ~ method_id + prep_batch,
      data = data.frame(method_id = res$method_id, prep_batch = res$prep_batch, has_qc = !is.na(m)),
      FUN = \(v) paste0(sum(v), "/", length(v))),
    lcs_rounding = rounding_log
  )
  list(template = out,
       lab_qc = ars_build_lab_qc(extract),
       case_narrative = extract$case_narrative,
       sample_receipt = extract$sample_receipt,
       checks = checks)
}

# Lab QC records in one long table: LCS, MS, MSD and lab duplicates (QC pages), method
# blanks, and surrogate recoveries. Column names follow `lab_qc_dat` in
# reference/dat_contract.md so ARS and SGS QC data share one structure. These rows never
# go to WQX. ARS prints a single RPD limit, written to rpd_limit_high.
ars_build_lab_qc <- function(extract) {
  res <- extract$results
  mb <- extract$method_blanks
  sur <- extract$surrogates
  qc <- extract$qc

  sdg <- unique(res[c("source_file", "workorder")])
  batch_lab <- unique(rbind(res[c("prep_batch", "performing_lab")], mb[c("prep_batch", "performing_lab")]))
  batch_lab <- batch_lab[!is.na(batch_lab$prep_batch) & !is.na(batch_lab$performing_lab), ]
  one_lab <- tapply(batch_lab$performing_lab, batch_lab$prep_batch,
                    \(v) if (length(unique(v)) == 1) v[1] else NA_character_)

  parts <- list()
  if (nrow(qc)) {
    parts$qc <- tibble::tibble(
      sample_type = qc$qc_type,
      qc_group = ifelse(qc$qc_type %in% c("LCS", "LCSD", "MS", "MSD"), "accuracy", "precision"),
      analyte = qc$analyte, analytical_method = qc$method_id,
      result = qc$qc_result, non_detect = NA, resultflag = qc$flag, repdl = NA_real_, mdl = NA_real_,
      units = qc$units, amount_spiked = qc$spike_level, initial_conc = qc$samp_result,
      percent_recovered = qc$recovery, rec_limit_low = qc$lcl, rec_limit_high = qc$ucl,
      sample_rpd = qc$rpd, rpd_limit_high = qc$rpd_limit,
      prp_batch = qc$prep_batch, sdg = sdg$workorder[match(qc$source_file, sdg$source_file)],
      lab_sample_id = NA_character_, sample_id = qc$parent_sample, run_date = as.Date(NA),
      lab_name = as.character(one_lab[qc$prep_batch]), source_file = qc$source_file, source_page = qc$source_page
    )
  }
  if (nrow(mb)) {
    parts$mb <- tibble::tibble(
      sample_type = "MB", qc_group = "blank", analyte = mb$analyte, analytical_method = mb$method_id,
      result = mb$result_value, non_detect = mb$non_detect, resultflag = mb$flags, repdl = mb$pql, mdl = mb$mdl,
      units = mb$units, amount_spiked = NA_real_, initial_conc = NA_real_, percent_recovered = NA_real_,
      rec_limit_low = NA_real_, rec_limit_high = NA_real_, sample_rpd = NA_real_, rpd_limit_high = NA_real_,
      prp_batch = mb$prep_batch, sdg = mb$workorder, lab_sample_id = mb$lab_sample_number,
      sample_id = NA_character_, run_date = mb$analysis_date, lab_name = mb$performing_lab,
      source_file = mb$source_file, source_page = mb$source_page
    )
  }
  if (nrow(sur)) {
    parts$sur <- tibble::tibble(
      sample_type = "SURR", qc_group = "surrogate", analyte = sur$analyte, analytical_method = sur$method_id,
      result = sur$result_value, non_detect = sur$non_detect, resultflag = sur$flags, repdl = NA_real_, mdl = NA_real_,
      units = sur$units, amount_spiked = NA_real_, initial_conc = NA_real_, percent_recovered = sur$pct_recovery,
      rec_limit_low = sur$lcl, rec_limit_high = sur$ucl, sample_rpd = NA_real_, rpd_limit_high = NA_real_,
      prp_batch = sur$prep_batch, sdg = sur$workorder, lab_sample_id = sur$lab_sample_number,
      sample_id = sur$client_sample_name, run_date = sur$analysis_date, lab_name = sur$performing_lab,
      source_file = sur$source_file, source_page = sur$source_page
    )
  }
  dplyr::bind_rows(parts)
}
