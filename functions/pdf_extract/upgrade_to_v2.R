# Bring a corrected hand-entered workbook (<name>_corrected.xlsx, made by correct_hand_entered())
# up to template v2, the layout the PDF extractor writes for later years. Writes a new
# <name>_v2.xlsx; the corrected file and the original are never modified.
#
# Changes (cells shaded yellow and listed on the new "v2_upgrade_log" sheet; shading from
# correct_hand_entered() is carried over):
#   1. Non-detect convention: Result Value blank, Result Qualifier "U", Result Detection
#      Condition "Not Detected" (a "J" result gets "Present Below Quantification Limit").
#   2. New columns "Sample Receipt Date" and "Sample Receipt Time" after "Analysis Start Time
#      Zone", taken from the primary receipt of the lab report that holds the row's work order.
#   3. Spike, Percent Recovered, LCL, UCL and Result Analytical Method ID filled from the PDF
#      where the key is blank.
#   4. Header "Percent Recoverd" respelled "Percent Recovered".
#   5. Time zones: Activity Start, Activity End and Analysis Start Time Zone set to "AKST" on
#      every row that has the matching time (all field observations are in Alaska).
#   6. Surrogate rows (analyte contains "(surr)") removed from the template sheet: they are lab
#      QC records and belong on the lab_qc sheet, never in the WQX upload. Rows with no lab
#      sample number and no analyte (stray rows) are removed too. Both are listed in the log.
#   7. New sheets: lab_qc, sample_receipt, case_narrative, accounting (from the extractor).
# The template sheet is rebuilt from values, so rows can be removed; other sheets are kept.
# Rows the PDF extract cannot be paired with keep their cells apart from the changes above.
#
# Needs: dplyr, readxl, openxlsx, tibble. Sources correct_hand_entered.R for the matching helpers.
# Contains nothing year-specific.

if (!exists("corr_norm")) {
  src <- file.path("functions", "pdf_extract", "correct_hand_entered.R")
  if (requireNamespace("here", quietly = TRUE)) src <- here::here(src)
  source(src)
}

upgrade_corrected_to_v2 <- function(corrected_path, extract, mapped, out_path = NULL,
                                    sheet = "KWF_Baseline_EPA_Template", overwrite = FALSE) {
  stopifnot(requireNamespace("openxlsx", quietly = TRUE), requireNamespace("readxl", quietly = TRUE))
  if (is.null(out_path)) out_path <- sub("_corrected(\\.xlsx)$", "_v2\\1", corrected_path)
  if (normalizePath(out_path, mustWork = FALSE) == normalizePath(corrected_path, mustWork = FALSE)) {
    stop("out_path must differ from corrected_path", call. = FALSE)
  }
  if (file.exists(out_path) && !overwrite) stop("File exists: ", out_path, call. = FALSE)

  txt <- readxl::read_excel(corrected_path, sheet = sheet, col_types = "text")
  d <- openxlsx::read.xlsx(corrected_path, sheet = sheet, skipEmptyRows = FALSE,
                           skipEmptyCols = FALSE, sep.names = " ")
  if (!identical(names(txt), names(d)) || nrow(txt) != nrow(d)) {
    stop("Typed and text reads of the key disagree; cannot continue", call. = FALSE)
  }
  pos <- match("Analysis Start Time Zone", names(txt))
  if (is.na(pos)) stop("Key lacks 'Analysis Start Time Zone'", call. = FALSE)
  if (any(c("Sample Receipt Date", "Sample Receipt Time") %in% names(txt))) stop("Key already has receipt columns", call. = FALSE)
  pr_old <- "Percent Recoverd"; pr_name <- "Percent Recovered"
  respelled <- pr_old %in% names(txt)
  if (respelled) { names(txt)[names(txt) == pr_old] <- pr_name; names(d)[names(d) == pr_old] <- pr_name }
  need <- c("Lab Sample Number", "Characteristic Name", "Activity Type", "Result Value",
            "Result Qualifier", "Result Detection Condition", "Result Analytical Method ID",
            "Spike", pr_name, "LCL", "UCL", "Activity ID")
  if (length(miss <- setdiff(need, names(txt)))) stop("Key lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  n <- nrow(txt)

  # ---- rows to remove: surrogates and stray rows ----
  cn_all <- txt[["Characteristic Name"]]
  surr <- !is.na(cn_all) & grepl("surr", cn_all, ignore.case = TRUE)
  stray <- is.na(cn_all) & is.na(txt[["Lab Sample Number"]])
  removed <- surr | stray
  keep <- which(!removed)

  # ---- pair key rows with the mapped template (lab sample number + analyte) ----
  t <- mapped$template
  tl <- corr_lab(t[["Lab Sample Number"]]); tan <- corr_norm(t[["Characteristic Name"]])
  if (anyDuplicated(data.frame(tl, tan))) stop("Mapped template repeats lab sample and analyte pairs; cannot match", call. = FALSE)
  r <- extract$results
  src_of <- function(lab, an) {
    i <- match(paste(lab, an), paste(corr_lab(r$lab_sample_base), corr_norm(r$analyte)))
    list(pdf = r$source_file[i], page = r$source_page[i])
  }
  klab <- corr_lab(txt[["Lab Sample Number"]]); kan <- corr_norm(cn_all)
  field <- !removed & !is.na(txt[["Activity Type"]])
  ti <- rep(NA_integer_, n)
  ti[field] <- match(paste(klab[field], kan[field]), paste(tl, tan))
  key_nd <- grepl("^<", txt[["Result Value"]]) | txt[["Result Qualifier"]] %in% c("ND", "U")
  map_nd <- t[["Result Detection Condition"]] %in% "Not Detected"

  # ---- shading and log ----
  shade <- data.frame(j = integer(), column = character(), stringsAsFactors = FALSE)
  prior <- tryCatch(readxl::read_excel(corrected_path, sheet = "corrections_log", col_types = "text"), error = \(e) NULL)
  if (!is.null(prior) && all(c("type", "sheet_row", "column") %in% names(prior))) {
    p <- prior[prior$type %in% c("changed", "added row") & !is.na(prior$sheet_row), ]
    pcol <- ifelse(p$column == "(whole row)", NA, ifelse(p$column == pr_old, pr_name, p$column))
    for (q in seq_len(nrow(p))) {
      j <- as.integer(p$sheet_row[q]) - 1L
      cols <- if (is.na(pcol[q])) names(txt) else pcol[q]
      shade <- rbind(shade, data.frame(j = j, column = cols, stringsAsFactors = FALSE))
    }
  }
  log <- list()
  add_log <- function(type, row, column, old, new, lab, analyte, pdf = NA, page = NA, note = NA) {
    log[[length(log) + 1]] <<- data.frame(type = type, corrected_row = row, v2_row = NA_integer_, column = column,
      old = as.character(old), new = as.character(new), lab_sample = lab, analyte = analyte,
      source_pdf = pdf, source_page = page, note = note, stringsAsFactors = FALSE)
  }
  set <- function(j, column, value, tj) {
    old <- if (column == "Result Value") txt[[column]][j] else d[[column]][j]
    d[[column]][j] <<- value
    shade[nrow(shade) + 1, ] <<- list(j, column)
    s <- src_of(klab[j], kan[j])
    add_log("changed", j + 1L, column, old, value, klab[j], t[["Characteristic Name"]][tj], s$pdf, s$page)
  }

  if (respelled) add_log("header respelled", 1L, pr_old, pr_old, pr_name, NA, NA)
  for (j in which(removed)) {
    add_log(if (surr[j]) "removed row: surrogate (on lab_qc)" else "removed row: stray, no lab sample or analyte",
            j + 1L, "(whole row)", paste(na.omit(c(cn_all[j], txt[["Result Value"]][j])), collapse = " = "), NA,
            txt[["Lab Sample Number"]][j], NA)
  }

  # ---- per-row changes for paired field rows ----
  for (j in which(!is.na(ti))) {
    tj <- ti[j]
    if (key_nd[j] != map_nd[tj]) {
      add_log("not changed: detect/non-detect disagree", j + 1L, "Result Value", txt[["Result Value"]][j],
              if (map_nd[tj]) "non-detect" else t[["Result Value"]][tj], klab[j], t[["Characteristic Name"]][tj])
      next
    }
    des_q <- t[["Result Qualifier"]][tj]; des_c <- t[["Result Detection Condition"]][tj]
    if (map_nd[tj] && !is.na(txt[["Result Value"]][j])) set(j, "Result Value", NA, tj)
    if (!is.na(des_q) && !identical(txt[["Result Qualifier"]][j], des_q)) set(j, "Result Qualifier", des_q, tj)
    if (!is.na(des_c) && !identical(txt[["Result Detection Condition"]][j], des_c)) set(j, "Result Detection Condition", des_c, tj)
    des_m <- t[["Result Analytical Method ID"]][tj]
    if (is.na(txt[["Result Analytical Method ID"]][j]) && !is.na(des_m)) set(j, "Result Analytical Method ID", des_m, tj)
    for (cn in c("Spike", pr_name, "LCL", "UCL")) {
      v <- t[[cn]][tj]
      if (is.na(txt[[cn]][j]) && !is.na(v)) set(j, cn, v, tj)
    }
  }

  # ---- time zones: AKST wherever the matching time exists ----
  has_lab <- !removed & !is.na(txt[["Lab Sample Number"]])
  for (pair in list(c("Activity Start Time", "Activity Start Time Zone"),
                    c("Activity End Time", "Activity End Time Zone"),
                    c("Analysis Start Time", "Analysis Start Time Zone"))) {
    if (!all(pair %in% names(d))) next
    idx <- which(has_lab & !is.na(txt[[pair[1]]]) & !(txt[[pair[2]]] %in% "AKST"))
    if (!length(idx)) next
    old <- ifelse(is.na(txt[[pair[2]]][idx]), "blank", txt[[pair[2]]][idx])
    d[[pair[2]]][idx] <- "AKST"
    shade <- rbind(shade, data.frame(j = idx, column = pair[2], stringsAsFactors = FALSE))
    tab <- table(old)
    add_log("changed", NA, pair[2], paste(paste0(names(tab), " (", tab, ")"), collapse = ", "), "AKST", NA, NA,
            note = paste0(length(idx), " row(s)"))
  }

  # ---- receipt columns ----
  sr <- mapped$sample_receipt[mapped$sample_receipt$role == "primary", ]
  wo <- toupper(sub("-.*$", "", txt[["Lab Sample Number"]]))
  si <- match(wo, toupper(sr$workorder))
  serial <- ifelse(has_lab & !is.na(si), as.numeric(sr$receipt_date[si]) - as.numeric(as.Date("1899-12-30")), NA_real_)
  tfrac <- vapply(seq_len(n), function(i) {
    x <- if (has_lab[i] && !is.na(si[i])) sr$receipt_time[si[i]] else NA_character_
    if (is.na(x)) return(NA_real_)
    p <- as.numeric(strsplit(x, ":")[[1]]); (p[1] * 3600 + p[2] * 60 + ifelse(is.na(p[3]), 0, p[3])) / 86400
  }, 1)
  d <- tibble::add_column(tibble::as_tibble(d), `Sample Receipt Date` = serial, `Sample Receipt Time` = tfrac,
                          .after = "Analysis Start Time Zone")
  shade <- rbind(shade, data.frame(j = which(!is.na(serial)), column = "Sample Receipt Date"),
                 data.frame(j = which(!is.na(tfrac)), column = "Sample Receipt Time"))
  by_wo <- split(which(has_lab), wo[has_lab])
  for (w in names(by_wo)) {
    i <- si[by_wo[[w]][1]]
    add_log("added column", NA, "Sample Receipt Date and Time", NA,
            if (is.na(i)) NA else paste(sr$receipt_date[i], sr$receipt_time[i]), w, NA,
            if (is.na(i)) NA else sr$source_file[i], NA,
            paste0(length(by_wo[[w]]), " row(s)", if (is.na(i)) "; no primary receipt found for this work order" else ""))
  }
  for (j in which(field & is.na(ti))) {
    add_log("not paired with PDF extract", j + 1L, "(whole row)", cn_all[j], NA, klab[j], NA)
  }

  # ---- rebuild the template sheet from values ----
  out <- d[keep, , drop = FALSE]
  rv <- out[["Result Value"]]
  if (is.character(rv)) {
    rn <- suppressWarnings(as.numeric(rv))
    if (all(is.na(rv) | !is.na(rn))) out[["Result Value"]] <- rn
  }
  wb <- openxlsx::loadWorkbook(corrected_path)
  pos_sheet <- match(sheet, names(wb))
  openxlsx::removeWorksheet(wb, sheet)
  openxlsx::addWorksheet(wb, sheet)
  ord <- append(setdiff(seq_along(names(wb)), length(names(wb))), length(names(wb)), after = pos_sheet - 1L)
  openxlsx::worksheetOrder(wb) <- ord
  openxlsx::writeData(wb, sheet, out, keepNA = FALSE, headerStyle = openxlsx::createStyle(textDecoration = "bold"))
  rows <- seq_len(nrow(out)) + 1L
  date_cols <- which(grepl("Date$", names(out)) & vapply(out, is.numeric, TRUE))
  time_cols <- which(grepl("Time$", names(out)) & vapply(out, is.numeric, TRUE))
  if (length(date_cols)) openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = "m/d/yyyy"), rows = rows, cols = date_cols, gridExpand = TRUE)
  if (length(time_cols)) openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = "h:mm"), rows = rows, cols = time_cols, gridExpand = TRUE)
  fill <- openxlsx::createStyle(fgFill = "#FFFF00")
  shade <- unique(shade)
  shade$row <- match(shade$j, keep) + 1L
  shade$col <- match(shade$column, names(out))
  shade <- shade[!is.na(shade$row) & !is.na(shade$col), ]
  for (cl in unique(shade$col)) {
    openxlsx::addStyle(wb, sheet, fill, rows = shade$row[shade$col == cl], cols = cl, stack = TRUE)
  }
  openxlsx::setColWidths(wb, sheet, cols = seq_along(out), widths = "auto")
  openxlsx::freezePane(wb, sheet, firstRow = TRUE)

  # ---- new sheets ----
  too_long <- vapply(mapped$case_narrative$narrative_text, nchar, 1L) > 32000
  if (any(too_long)) stop("A case narrative exceeds the Excel cell limit.", call. = FALSE)
  add_sheet <- function(name, df) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, df, headerStyle = openxlsx::createStyle(textDecoration = "bold"))
  }
  log_df <- if (length(log)) do.call(rbind, log) else data.frame(type = character())
  log_df$v2_row <- match(log_df$corrected_row - 1L, keep) + 1L
  add_sheet("v2_upgrade_log", log_df)
  add_sheet("lab_qc", mapped$lab_qc)
  add_sheet("case_narrative", mapped$case_narrative)
  add_sheet("sample_receipt", mapped$sample_receipt)
  add_sheet("accounting", extract$accounting)

  openxlsx::saveWorkbook(wb, out_path, overwrite = overwrite)
  message("Wrote ", out_path)
  invisible(list(path = out_path, log = tibble::as_tibble(log_df)))
}
