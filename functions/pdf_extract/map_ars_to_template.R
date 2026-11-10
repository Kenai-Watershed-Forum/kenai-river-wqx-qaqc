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
  if (anyDuplicated(qc$qc_key)) {
    stop("Conflicting ", cfg$recovery_qc_type, " records for: ",
         paste(unique(qc$qc_key[duplicated(qc$qc_key)]), collapse = "; "), call. = FALSE)
  }
  m <- match(res$qc_key, qc$qc_key)

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
      FUN = \(v) paste0(sum(v), "/", length(v)))
  )
  list(template = out, checks = checks)
}
