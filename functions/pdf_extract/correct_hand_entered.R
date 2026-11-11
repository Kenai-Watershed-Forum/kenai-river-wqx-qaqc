# Make a corrected copy of a hand-entered 72-column workbook, using the Stage 1 extract of
# the lab PDFs as the reference. Only cells where the PDF disagrees are changed; the key's own
# conventions (non-detect style, analyte spellings such as "m,p-Xylene", date cells) are kept.
# Changed cells are shaded yellow, rows the PDF has and the key lacks are appended, and every
# change is listed on a new "corrections_log" sheet. The original file is never modified.
#
# Needs: dplyr, readxl, openxlsx. Contains nothing year-specific.

# Known spelling errors in hand-entered analyte names (key spelling -> normalized PDF spelling)
key_name_typos <- c("1112tretrachlorethane" = "1112tetrachloroethane")

corr_norm <- function(x) {
  x <- gsub("[^a-z0-9]", "", tolower(x))
  x <- sub("xylenes$", "xylene", x)
  ifelse(x %in% names(key_name_typos), key_name_typos[x], x)
}
corr_lab <- function(x) sub("[A-Za-z]$", "", x)
corr_serial <- function(d) as.numeric(d) - as.numeric(as.Date("1899-12-30"))

correct_hand_entered <- function(key_path, extract, out_path = NULL,
                                 sheet = "KWF_Baseline_EPA_Template") {
  stopifnot(requireNamespace("openxlsx", quietly = TRUE), requireNamespace("readxl", quietly = TRUE))
  if (is.null(out_path)) {
    out_path <- file.path(dirname(key_path), sub("(\\.xlsx)$", "_corrected\\1", basename(key_path)))
  }
  if (normalizePath(out_path, mustWork = FALSE) == normalizePath(key_path, mustWork = FALSE)) {
    stop("out_path must differ from key_path", call. = FALSE)
  }

  key <- readxl::read_excel(key_path, sheet = sheet, col_types = "text")
  need <- c("Lab Sample Number", "Characteristic Name", "Result Value", "Result Qualifier",
            "Result Detection Limit Value 1", "Result Detection Limit Value 2",
            "Analysis Start Date", "Activity Type")
  if (length(miss <- setdiff(need, names(key)))) stop("Key lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)

  lt_style <- any(grepl("^<", key[["Result Value"]]), na.rm = TRUE)

  k <- tibble::tibble(
    row = seq_len(nrow(key)) + 1L,                       # sheet row (header is row 1)
    lab = corr_lab(key[["Lab Sample Number"]]),
    an = corr_norm(key[["Characteristic Name"]]),
    nd = grepl("^<", key[["Result Value"]]) | key[["Result Qualifier"]] %in% c("ND", "U"),
    val = suppressWarnings(as.numeric(sub("^<", "", key[["Result Value"]]))),
    l1 = suppressWarnings(as.numeric(key[["Result Detection Limit Value 1"]])),
    l2 = suppressWarnings(as.numeric(key[["Result Detection Limit Value 2"]])),
    adate = as.Date(suppressWarnings(as.numeric(key[["Analysis Start Date"]])), origin = "1899-12-30"),
    field = !is.na(key[["Activity Type"]]) & !grepl("surr", key[["Characteristic Name"]], ignore.case = TRUE))
  k <- k[k$field, ]

  r <- extract$results
  e <- tibble::tibble(
    lab = corr_lab(r$lab_sample_base), an = corr_norm(r$analyte), name = r$analyte,
    nd = r$non_detect, val = r$result_value, l1 = r$pql, l2 = r$mdl, adate = r$analysis_date,
    pdf = r$source_file, page = r$source_page)
  if (anyDuplicated(e[c("lab", "an")])) stop("Extract has repeated lab sample and analyte pairs; cannot match", call. = FALSE)

  # Pair on lab sample number and analyte
  k$ei <- match(paste(k$lab, k$an), paste(e$lab, e$an))

  # Lab sample numbers that match nothing: pair a key sample with an extract sample that has the
  # same analytes and values and is itself unmatched (catches mistyped sample numbers)
  lab_fix <- list()
  bad_labs <- setdiff(unique(k$lab[is.na(k$ei)]), character())
  bad_labs <- bad_labs[vapply(bad_labs, \(L) all(is.na(k$ei[k$lab == L])), TRUE)]
  free_e <- setdiff(unique(e$lab), unique(k$lab))
  sig <- function(d) paste(sort(paste(d$an, ifelse(d$nd, "nd", format(d$val, digits = 10)))), collapse = ";")
  for (L in bad_labs) {
    ks <- sig(k[k$lab == L, ])
    cand <- free_e[vapply(free_e, \(E) sig(e[e$lab == E, ]) == ks, TRUE)]
    if (length(cand) == 1) { lab_fix[[L]] <- cand; free_e <- setdiff(free_e, cand) }
  }
  for (L in names(lab_fix)) {
    idx <- k$lab == L
    k$ei[idx] <- match(paste(lab_fix[[L]], k$an[idx]), paste(e$lab, e$an))
  }

  wb <- openxlsx::loadWorkbook(key_path)
  col_of <- \(nm) match(nm, names(key))
  fill <- openxlsx::createStyle(fgFill = "#FFFF00")
  log <- list()
  add_log <- function(type, row, column, old, new, i) {
    log[[length(log) + 1]] <<- data.frame(type = type, sheet_row = row, column = column,
      old = as.character(old), new = as.character(new), lab_sample = e$lab[i], analyte = e$name[i],
      source_pdf = e$pdf[i], source_page = e$page[i], stringsAsFactors = FALSE)
  }
  put <- function(row, column, value, old, i, type = "changed") {
    openxlsx::writeData(wb, sheet, value, startCol = col_of(column), startRow = row, colNames = FALSE)
    openxlsx::addStyle(wb, sheet, fill, rows = row, cols = col_of(column), stack = TRUE, gridExpand = FALSE)
    add_log(type, row, column, old, value, i)
  }
  same <- \(a, b, tol = 1e-9) isTRUE(abs(a - b) <= tol * max(1, abs(b)))

  # Corrections to paired rows
  for (j in which(!is.na(k$ei))) {
    i <- k$ei[j]; row <- k$row[j]
    if (!is.na(k$lab[j]) && k$lab[j] %in% names(lab_fix)) {
      old <- key[["Lab Sample Number"]][row - 1]
      put(row, "Lab Sample Number", paste0(lab_fix[[k$lab[j]]], sub("^.*-\\d+", "", old)), old, i)
    }
    raw_name <- key[["Characteristic Name"]][row - 1]
    if (gsub("[^a-z0-9]", "", tolower(raw_name)) %in% names(key_name_typos)) {
      put(row, "Characteristic Name", e$name[i], key[["Characteristic Name"]][row - 1], i)
    }
    if (!is.na(e$adate[i]) && !identical(k$adate[j], e$adate[i])) {
      put(row, "Analysis Start Date", corr_serial(e$adate[i]), key[["Analysis Start Date"]][row - 1], i)
    }
    if (!is.na(e$l1[i]) && !same(k$l1[j], e$l1[i], 1e-6)) {
      put(row, "Result Detection Limit Value 1", e$l1[i], key[["Result Detection Limit Value 1"]][row - 1], i)
    }
    if (!is.na(e$l2[i]) && !same(k$l2[j], e$l2[i], 1e-6)) {
      put(row, "Result Detection Limit Value 2", e$l2[i], key[["Result Detection Limit Value 2"]][row - 1], i)
    }
    if (k$nd[j] != e$nd[i]) {
      add_log("not changed: detect/non-detect disagree", row, "Result Value",
              key[["Result Value"]][row - 1], if (e$nd[i]) "non-detect" else e$val[i], i)
    } else if (e$nd[i]) {
      if (lt_style && !is.na(e$l1[i]) && !same(k$val[j], e$l1[i])) {
        put(row, "Result Value", paste0("<", format(e$l1[i], scientific = FALSE)), key[["Result Value"]][row - 1], i)
      }
    } else if (!same(k$val[j], e$val[i])) {
      put(row, "Result Value", e$val[i], key[["Result Value"]][row - 1], i)
    }
  }

  # Rows the PDF has and the key lacks: copy a donor row from the same lab sample and analysis date
  paired_e <- unique(k$ei[!is.na(k$ei)])
  new_e <- setdiff(seq_len(nrow(e)), paired_e)
  new_e <- new_e[e$lab[new_e] %in% k$lab | e$lab[new_e] %in% unlist(lab_fix)]
  next_row <- nrow(key) + 2L
  blank_cols <- intersect(c("Spike", "Percent Recoverd", "Percent Recovered", "LCL", "UCL", "Result Comment"), names(key))
  datestyle <- openxlsx::createStyle(numFmt = "m/d/yyyy")
  for (i in new_e) {
    lab_key <- names(lab_fix)[unlist(lab_fix) == e$lab[i]]
    L <- if (length(lab_key)) lab_key else e$lab[i]
    donor <- which(k$lab == L & k$adate == e$adate[i])[1]
    if (is.na(donor)) donor <- which(k$lab == L)[1]
    if (is.na(donor)) { add_log("not added: no donor row", NA, NA, NA, NA, i); next }
    vals <- as.list(key[k$row[donor] - 1, ])
    vals[["Characteristic Name"]] <- e$name[i]
    vals[["Analysis Start Date"]] <- corr_serial(e$adate[i])
    vals[["Result Detection Limit Value 1"]] <- e$l1[i]
    vals[["Result Detection Limit Value 2"]] <- e$l2[i]
    vals[["Result Value"]] <- if (e$nd[i]) { if (lt_style) paste0("<", format(e$l1[i], scientific = FALSE)) else NA } else e$val[i]
    vals[["Result Qualifier"]] <- if (e$nd[i]) { if (lt_style) NA else "ND" } else NA
    for (b in blank_cols) vals[[b]] <- NA
    if (length(lab_key)) vals[["Lab Sample Number"]] <- paste0(e$lab[i], sub("^.*-\\d+", "", vals[["Lab Sample Number"]]))
    for (nm in names(vals)) {
      v <- vals[[nm]]
      if (!is.na(v)) {
        num <- suppressWarnings(as.numeric(v))
        openxlsx::writeData(wb, sheet, if (!is.na(num) && !grepl("^[A-Za-z<]", v)) num else v,
                            startCol = col_of(nm), startRow = next_row, colNames = FALSE)
      }
    }
    openxlsx::addStyle(wb, sheet, datestyle, rows = next_row, cols = col_of("Analysis Start Date"), stack = TRUE)
    openxlsx::addStyle(wb, sheet, fill, rows = next_row, cols = seq_along(names(key)), gridExpand = TRUE, stack = TRUE)
    add_log("added row", next_row, "(whole row)", NA, e$name[i], i)
    next_row <- next_row + 1L
  }

  # Key field rows with no counterpart in the extract
  unmatched_k <- k[is.na(k$ei), ]
  for (j in seq_len(nrow(unmatched_k))) {
    log[[length(log) + 1]] <- data.frame(type = "key row not found in PDF extract", sheet_row = unmatched_k$row[j],
      column = "(whole row)", old = key[["Characteristic Name"]][unmatched_k$row[j] - 1], new = NA,
      lab_sample = unmatched_k$lab[j], analyte = NA, source_pdf = NA, source_page = NA, stringsAsFactors = FALSE)
  }

  log_df <- if (length(log)) do.call(rbind, log) else data.frame(type = character())
  openxlsx::addWorksheet(wb, "corrections_log")
  openxlsx::writeData(wb, "corrections_log", log_df)
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  message("Wrote ", out_path)
  invisible(list(path = out_path, log = tibble::as_tibble(log_df)))
}
