# Write the mapped output of one sampling event to an .xlsx workbook.
#
# write_template_workbook(mapped, extract, path)
#   mapped   result of map_ars_to_template()
#   extract  result of extract_ars_dir() (for the accounting sheet)
#   path     output .xlsx; an existing file is never overwritten unless overwrite = TRUE
#
# Sheets: KWF_Baseline_EPA_Template (field results, bound for WQX), lab_qc (lab QC records,
# never bound for WQX), case_narrative (narrative text per report), sample_receipt (receipt
# events with temperature), accounting, and the mapper's check tables.

write_template_workbook <- function(mapped, extract, path, overwrite = FALSE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Package 'openxlsx' is required.", call. = FALSE)
  if (file.exists(path) && !overwrite) stop("File exists: ", path, call. = FALSE)

  sheets <- list(
    KWF_Baseline_EPA_Template = mapped$template,
    lab_qc = mapped$lab_qc,
    case_narrative = mapped$case_narrative,
    sample_receipt = mapped$sample_receipt,
    accounting = extract$accounting,
    sample_classes = mapped$checks$sample_classes,
    qualifier_vs_flag = mapped$checks$qualifier_vs_flag,
    recovery_coverage = mapped$checks$recovery_coverage
  )
  if (!is.null(mapped$checks$lcs_rounding)) sheets$lcs_rounding <- mapped$checks$lcs_rounding

  too_long <- vapply(mapped$case_narrative$narrative_text, nchar, 1L) > 32000
  if (any(too_long)) stop("A case narrative exceeds the Excel cell limit.", call. = FALSE)

  openxlsx::write.xlsx(sheets, path, overwrite = overwrite, headerStyle = openxlsx::createStyle(textDecoration = "bold"))
  invisible(path)
}
