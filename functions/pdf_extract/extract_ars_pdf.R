# Stage 1 of the lab-report PDF extraction: read ARS Aleut report PDFs and
# return what the PDF states, one row per analyte per test block. Nothing
# project-specific is assumed (no site lists, no WQX names, no year).
#
# extract_ars_pdf(path)  one PDF -> list(results, method_blanks, surrogates, qc, accounting, by_sample,
#                                        case_narrative, sample_receipt)
# extract_ars_dir(dir)   all PDFs in a folder, bound, with accounting
#
# Stops on any analyte-table line it cannot parse, so a layout change is never
# silently dropped.
# ---- helpers ---------------------------------------------------------------
# Split a line into "Label: value" segments for a known label set. Labels can
# share a line (left and right columns), so values run to the next label.
ars_fields <- function(line, labels) {
  pos <- vapply(labels, \(l) as.integer(regexpr(paste0(l, ":"), line, fixed = TRUE)), 1L)
  pos <- sort(pos[pos > 0])
  if (!length(pos)) return(character())
  starts <- pos + nchar(names(pos)) + 1
  ends <- c(pos[-1] - 1, nchar(line))
  stats::setNames(trimws(substring(line, starts, ends)), names(pos))
}
ars_num <- function(x) {
  x <- gsub("[,<>]", "", x)
  suppressWarnings(as.numeric(ifelse(x %in% c("ND", "NA", ""), NA, x)))
}
# Parse lab date-time text; stops if non-blank text does not parse.
ars_datetime <- function(x, what) {
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  out <- suppressWarnings(
    lubridate::parse_date_time(x, c("mdY IMSp", "mdY HM", "mdY"), tz = "UTC", quiet = TRUE)
  )
  bad <- !is.na(x) & is.na(out)
  if (any(bad)) stop("Unparseable ", what, ": ", paste(unique(x[bad]), collapse = "; "), call. = FALSE)
  out
}
# "200. 7 - Metals by ICP" -> "200.7"
ars_method_id <- function(x) {
  id <- sub("\\s+-\\s+.*$", "", x)
  gsub("\\s+", "", id)
}
ars_sample_labels <- c("Report Section", "Client Sample Name", "Matrix", "Collection Date",
                       "Workorder (SDG)", "Project")
ars_block_labels <- c("Lab Sample Number", "Analysis Date", "Prep Date", "Instrument",
                      "Analytical Method ID", "File Name", "Prep Method ID", "Dilution Factor",
                      "Prep Batch Number", "Report Basis", "Analyst Initials",
                      "Sample prep wt./vol", "Prep Extract Vol", "pH on receipt")
ars_row_pattern <- paste0(
  "^\\s*(.+?)\\s+",                                   # analyte
  "(?:(\\d{2,7}-\\d{2}-?\\d(?:/[\\d-]*)?)\\s+)?",  # CAS (optional; lab prints "108-38-3/106-" for m&p Xylenes)
  "(ND|NA|[<>]?-?[\\d,]*\\.?\\d+(?:[eE][+-]?\\d+)?)\\s+", # result
  "(?:([A-Za-z]{1,4}(?:\\s[A-Za-z]{1,4})*)\\s+)?",    # flags (optional)
  "(\\S+)\\s+([\\d,.]+)\\s+([\\d,.]+)(?:\\s+\\d+)?\\s*$"  # units, PQL, MDL, run #
)
# Surrogate recovery table (BTEX/VOC): analyte, CAS, result, flags, units, % recovery, LCL, UCL
ars_surrogate_pattern <- paste0(
  "^\\s*(.+?)\\s+",
  "(?:(\\d{2,7}-\\d{2}-?\\d\\w?)\\s+)?",
  "(ND|NA|[<>]?-?[\\d,]*\\.?\\d+)\\s+",
  "(?:([A-Za-z]{1,4}(?:\\s[A-Za-z]{1,4})*)\\s+)?",
  "(\\S+)\\s+(-?[\\d,.]+)\\s+(-?[\\d,.]+)\\s+(-?[\\d,.]+)(?:\\s+\\d+)?\\s*$"
)
ars_surrogate_noresult_pattern <- paste0(
  "^\\s*(.+?)\\s+",
  "(?:(\\d{2,7}-\\d{2}-?\\d\\w?)\\s+)?",
  "(-?[\\d,.]+)\\s+(-?[\\d,.]+)\\s+(-?[\\d,.]+)(?:\\s+\\d+)?\\s*$"
)
# ---- cover page --------------------------------------------------------------
ars_cover <- function(lines_by_page) {
  txt <- unlist(lines_by_page)
  grab <- \(p) {
    m <- regmatches(txt, regexec(p, txt))
    m <- m[lengths(m) > 0]
    if (length(m)) trimws(m[[1]][2]) else NA_character_
  }
  start <- grep("Sample Identification", txt)[1]
  end <- grep("Enclosed are", txt)[1]
  listed <- tibble::tibble(lab_sample_base = character(), client_description = character())
  if (!is.na(start) && !is.na(end)) {
    pat <- "[A-Z]\\d{6,8}-\\d{2}\\s+.+?(?=\\s{2,}[A-Z]\\d{6,8}-\\d{2}\\s|\\s*$)"
    hits <- unlist(regmatches(txt[(start + 1):(end - 1)],
                              gregexpr(pat, txt[(start + 1):(end - 1)], perl = TRUE)))
    listed <- tibble::tibble(
      lab_sample_base = sub("\\s.*$", "", hits),
      client_description = trimws(sub("^\\S+\\s+", "", hits))
    )
  }
  list(
    workorder = grab("Work Order #:\\s*(\\S+)"),
    work_id = grab("Work ID:\\s*(.+?)\\s*$"),
    date_received = grab("Date Received:\\s*(\\S+)"),
    listed = listed
  )
}
# ---- sample and method blank pages ---------------------------------------------
ars_parse_samples <- function(lines_by_page, file) {
  ctx <- list()
  blk <- list(); pending_lab <- NULL
  in_table <- FALSE; tbl <- "analyte"
  rows <- list()
  unparsed <- list()
  for (pi in seq_along(lines_by_page)) {
    txt <- paste(lines_by_page[[pi]], collapse = "\n")
    is_qc <- grepl("QUALITY CONTROL REPORT", txt, fixed = TRUE)
    is_sample <- !is_qc && grepl("The following test was conducted by|Analyte\\s+CASNo", txt)
    if (!is_sample) { in_table <- FALSE; next }
    for (ln in lines_by_page[[pi]]) {
      if (grepl("^\\s*The following test was conducted by:", ln)) {
        pending_lab <- trimws(sub("^.*conducted by:", "", ln))
        in_table <- FALSE
        next
      }
      if (grepl("^\\s*Analyte\\s+CASNo", ln)) { in_table <- TRUE; tbl <- "analyte"; next }
      if (grepl("^\\s*Surrogate\\s+CASNo", ln)) { in_table <- TRUE; tbl <- "surrogate"; sur_noresult <- !grepl("Result", ln); next }
      # A new block header ends the table (several blocks can follow without a blank line)
      if (in_table && grepl("^\\s*Lab Sample Number:", ln)) in_table <- FALSE
      if (in_table) {
        if (!nzchar(trimws(ln)) || grepl("^\\s*Page \\d+ of \\d+", ln)) { in_table <- FALSE; next }
        if (tbl == "surrogate") {
          # Layout without a Result column: analyte, CAS, % recovery, LCL, UCL
          if (isTRUE(sur_noresult)) {
            mn <- regmatches(ln, regexec(ars_surrogate_noresult_pattern, ln, perl = TRUE))[[1]]
            if (length(mn)) {
              rows[[length(rows) + 1]] <- c(
                ctx, blk,
                list(source_file = file, source_page = pi, row_type = "surrogate",
                     analyte = mn[2], cas = ifelse(nzchar(mn[3]), mn[3], NA_character_),
                     result_text = NA_character_, flags = NA_character_,
                     units = "%", pct_recovery = mn[4], lcl = mn[5], ucl = mn[6])
              )
              next
            }
          }
          ms <- regmatches(ln, regexec(ars_surrogate_pattern, ln, perl = TRUE))[[1]]
          if (length(ms)) {
            rows[[length(rows) + 1]] <- c(
              ctx, blk,
              list(source_file = file, source_page = pi, row_type = "surrogate",
                   analyte = ms[2], cas = ifelse(nzchar(ms[3]), ms[3], NA_character_),
                   result_text = ms[4], flags = ifelse(nzchar(ms[5]), ms[5], NA_character_),
                   units = ms[6], pct_recovery = ms[7], lcl = ms[8], ucl = ms[9])
            )
            next
          }
        }
        m <- regmatches(ln, regexec(ars_row_pattern, ln, perl = TRUE))[[1]]
        if (!length(m)) {
          unparsed[[length(unparsed) + 1]] <- sprintf("%s p%d: %s", file, pi, trimws(ln))
          next
        }
        rows[[length(rows) + 1]] <- c(
          ctx, blk,
          list(source_file = file, source_page = pi, row_type = "analyte",
               analyte = m[2], cas = ifelse(nzchar(m[3]), m[3], NA_character_),
               result_text = m[4], flags = ifelse(nzchar(m[5]), m[5], NA_character_),
               units = m[6], pql = m[7], mdl = m[8])
        )
        next
      }
      f <- ars_fields(ln, ars_sample_labels)
      if (length(f)) {
        
        for (n in names(f)) ctx[[n]] <- f[[n]]
      }
      f <- ars_fields(ln, ars_block_labels)
      if ("Lab Sample Number" %in% names(f)) {
        blk <- list(performing_lab = if (is.null(pending_lab)) blk$performing_lab else pending_lab)
        pending_lab <- NULL
      }
      for (n in names(f)) blk[[n]] <- f[[n]]
    }
  }
  if (length(unparsed)) {
    stop("Unparsed analyte-table lines:\n", paste(unlist(unparsed), collapse = "\n"), call. = FALSE)
  }
  if (!length(rows)) return(tibble::tibble())
  get <- \(x, nm) vapply(rows, \(r) if (is.null(r[[nm]]) || !nzchar(r[[nm]])) NA_character_ else r[[nm]], "")
  out <- tibble::tibble(
    source_file = file,
    source_page = vapply(rows, \(r) as.integer(r$source_page), 1L),
    report_section = get(rows, "Report Section"),
    workorder = get(rows, "Workorder (SDG)"),
    project = get(rows, "Project"),
    client_sample_name = get(rows, "Client Sample Name"),
    matrix = get(rows, "Matrix"),
    collect_text = get(rows, "Collection Date"),
    performing_lab = get(rows, "performing_lab"),
    lab_sample_number = get(rows, "Lab Sample Number"),
    analysis_text = get(rows, "Analysis Date"),
    prep_text = get(rows, "Prep Date"),
    instrument = get(rows, "Instrument"),
    method_id_raw = get(rows, "Analytical Method ID"),
    prep_method_id = get(rows, "Prep Method ID"),
    prep_batch = get(rows, "Prep Batch Number"),
    dilution_factor = get(rows, "Dilution Factor"),
    report_basis = get(rows, "Report Basis"),
    analyst = get(rows, "Analyst Initials"),
    prep_wt_vol = get(rows, "Sample prep wt./vol"),
    ph_on_receipt = get(rows, "pH on receipt"),
    analyte = get(rows, "analyte"),
    cas = get(rows, "cas"),
    result_text = get(rows, "result_text"),
    flags = get(rows, "flags"),
    units = get(rows, "units"),
    pql_text = get(rows, "pql"),
    mdl_text = get(rows, "mdl"),
    row_type = get(rows, "row_type"),
    pct_recovery = ars_num(get(rows, "pct_recovery")),
    lcl = ars_num(get(rows, "lcl")),
    ucl = ars_num(get(rows, "ucl"))
  )
  required <- c("report_section", "client_sample_name", "collect_text", "lab_sample_number",
                "analysis_text", "method_id_raw")
  # Method blank pages print no client sample name
  for (col in required) {
    miss <- is.na(out[[col]])
    if (col == "client_sample_name") miss <- miss & out$report_section != "Method Blank Report"
    if (any(miss)) {
      stop("Missing '", col, "' for ", sum(miss), " analyte row(s) in ", file,
           " (pages ", paste(unique(out$source_page[miss]), collapse = ", "), ")",
           call. = FALSE)
    }
  }
  coll <- ars_datetime(out$collect_text, "Collection Date")
  anal <- ars_datetime(out$analysis_text, "Analysis Date")
  prep <- ars_datetime(out$prep_text, "Prep Date")
  out |>
    dplyr::mutate(
      lab_sample_base = sub("[A-Z]$", "", lab_sample_number),
      method_id = ars_method_id(sub("\\s*File Name:.*$", "", method_id_raw)),
      collect_date = as.Date(coll), collect_time = format(coll, "%H:%M:%S"),
      analysis_date = as.Date(anal), analysis_time = format(anal, "%H:%M:%S"),
      prep_date = as.Date(prep), prep_time = ifelse(is.na(prep), NA, format(prep, "%H:%M:%S")),
      result_value = ars_num(result_text),
      non_detect = result_text == "ND" | grepl("^<", result_text),
      pql = ars_num(pql_text), mdl = ars_num(mdl_text),
      dilution_factor = ars_num(dilution_factor)
    ) |>
    dplyr::relocate(lab_sample_base, method_id, collect_date, collect_time,
                    analysis_date, analysis_time, prep_date, prep_time,
                    result_value, non_detect, pql, mdl, .after = lab_sample_number) |>
    dplyr::select(-collect_text, -analysis_text, -prep_text, -pql_text, -mdl_text)
}
# ---- quality control pages -------------------------------------------------------
ars_qc_labels <- c("Analysis", "MB", "Base Sample", "Parent", "Prep Date", "Samp. Anal. Date",
                   "DUP Anal. Date", "MB Anal. Date", "LCS Anal. Date", "MS Anal. Date",
                   "MSD Anal. Date", "Units", "Matrix")
ars_qc_row <- function(ln, type) {
  # Row with no spike or result values (lab prints only ND and a 0- 0 limit)
  if (grepl("\\bND\\s+0\\s*-\\s*0\\s*$", ln)) return(list())
  tok <- strsplit(trimws(ln), "\\s+")[[1]]
  is_val <- grepl("^(ND|NA|[<>]?-?[\\d,]*\\.?\\d+(?:[eE][+-]?\\d+)?)$", tok, perl = TRUE)
  first <- which(is_val)[1]
  if (is.na(first) || first == 1) return(NULL)
  analyte <- paste(tok[seq_len(first - 1)], collapse = " ")
  rest <- tok[first:length(tok)]
  dash <- which(rest == "-")
  lcl <- ucl <- NA_real_
  if (length(dash) == 1 && dash > 1 && dash < length(rest)) {
    lcl <- ars_num(rest[dash - 1]); ucl <- ars_num(rest[dash + 1])
    nums <- rest[seq_len(dash - 2)]
    tail <- rest[-seq_len(dash + 1)]
  } else {
    nums <- rest[is_val[first:length(tok)]]
    tail <- rest[!is_val[first:length(tok)]]
  }
  n <- ars_num(nums)
  dup_limit <- NA_real_
  if (type == "DUP" && length(n) == 4) { dup_limit <- n[4]; n <- n[1:3] }
  rpd_limit <- if (length(tail) && grepl("^[\\d.]+$", tail[1], perl = TRUE)) ars_num(tail[1]) else NA_real_
  if (!is.na(dup_limit)) rpd_limit <- dup_limit
  flag <- paste(if (is.na(rpd_limit) || !is.na(dup_limit)) tail else tail[-1], collapse = " ")
  flag <- ifelse(nzchar(flag), flag, NA_character_)
  if (type == "LCS" && length(n) == 4) {
    list(list(qc_type = "LCS", analyte = analyte, samp_result = n[1], qc_result = n[2],
              spike_level = n[3], recovery = n[4], lcl = lcl, ucl = ucl,
              rpd = NA_real_, rpd_limit = rpd_limit, flag = flag))
  } else if (type == "MS" && length(n) == 4) {
    list(list(qc_type = "MS", analyte = analyte, samp_result = n[1], qc_result = n[2],
              spike_level = n[3], recovery = n[4], lcl = lcl, ucl = ucl,
              rpd = NA_real_, rpd_limit = rpd_limit, flag = flag))
  } else if (type == "MS/MSD" && length(n) %in% c(7, 8)) {
    list(list(qc_type = "MS", analyte = analyte, samp_result = n[1], qc_result = n[2],
              spike_level = n[4], recovery = n[6], lcl = lcl, ucl = ucl,
              rpd = if (length(n) == 8) n[8] else NA_real_, rpd_limit = rpd_limit, flag = flag),
         list(qc_type = "MSD", analyte = analyte, samp_result = n[1], qc_result = n[3],
              spike_level = n[5], recovery = n[7], lcl = lcl, ucl = ucl,
              rpd = if (length(n) == 8) n[8] else NA_real_, rpd_limit = rpd_limit, flag = flag))
  } else if (type == "DUP" && length(n) == 3) {
    list(list(qc_type = "DUP", analyte = analyte, samp_result = n[1], qc_result = n[2],
              spike_level = NA_real_, recovery = NA_real_, lcl = NA_real_, ucl = NA_real_,
              rpd = n[3], rpd_limit = rpd_limit, flag = flag))
  } else if (type == "LCS/LCSD" && length(n) %in% c(7, 8)) {
    list(list(qc_type = "LCS", analyte = analyte, samp_result = n[1], qc_result = n[2],
              spike_level = n[4], recovery = n[6], lcl = lcl, ucl = ucl,
              rpd = if (length(n) == 8) n[8] else NA_real_, rpd_limit = rpd_limit, flag = flag),
         list(qc_type = "LCSD", analyte = analyte, samp_result = n[1], qc_result = n[3],
              spike_level = n[5], recovery = n[7], lcl = lcl, ucl = ucl,
              rpd = if (length(n) == 8) n[8] else NA_real_, rpd_limit = rpd_limit, flag = flag))
  } else if (type %in% c("LCS", "MS") && length(n) == 2) {
    # Lab left spike level and recovery blank; keep result and limits
    list(list(qc_type = type, analyte = analyte, samp_result = n[1], qc_result = n[2],
              spike_level = NA_real_, recovery = NA_real_, lcl = lcl, ucl = ucl,
              rpd = NA_real_, rpd_limit = rpd_limit, flag = flag))
  } else {
    stop("Unrecognised QC row for ", type, ": ", trimws(ln), call. = FALSE)
  }
}
ars_parse_qc <- function(lines_by_page, file) {
  rows <- list()
  type <- NULL; batch <- NA_character_; hdr <- list(); in_table <- FALSE
  type_map <- c("LCS REPORT" = "LCS", "SAMPLE DUPLICATE REPORT" = "DUP",
                "MS REPORT" = "MS", "MS/MSD REPORT" = "MS/MSD", "LCS/LCSD REPORT" = "LCS/LCSD")
  for (pi in seq_along(lines_by_page)) {
    if (!any(grepl("QUALITY CONTROL REPORT", lines_by_page[[pi]], fixed = TRUE))) next
    for (ln in lines_by_page[[pi]]) {
      t <- trimws(ln)
      if (grepl("^Prep Batch:", t)) { batch <- trimws(sub("^Prep Batch:", "", t)); next }
      # Serial dilution tables are not recorded; any other unknown report header stops the run
      if (grepl("^[A-Z][A-Z/ ]* REPORT$", t) && t != "QUALITY CONTROL REPORT" && !grepl("^FOOTNOTES", t) && !t %in% names(type_map)) {
        if (t == "SERIAL DILUTION REPORT") { type <- NULL; in_table <- FALSE; next }
        stop(file, ": unknown QC report type: ", t, call. = FALSE)
      }
      if (t %in% names(type_map)) { type <- type_map[[t]]; hdr <- list(); in_table <- FALSE; next }
      if (grepl("^FOOTNOTES TO QC REPORT", t)) { in_table <- FALSE; type <- NULL; next }
      if (is.null(type)) next
      if (grepl("^Analyte Name", t)) { in_table <- TRUE; next }
      if (in_table) {
        if (!nzchar(t)) { in_table <- FALSE; next }
        parsed <- ars_qc_row(ln, type)
        for (p in parsed) {
          rows[[length(rows) + 1]] <- c(
            list(source_file = file, source_page = pi, prep_batch = batch,
                 analysis = hdr[["Analysis"]] %||% NA_character_,
                 parent_sample = hdr[["Base Sample"]] %||% hdr[["Parent"]] %||% NA_character_,
                 prep_text = hdr[["Prep Date"]] %||% NA_character_,
                 units = hdr[["Units"]] %||% NA_character_),
            p)
        }
        next
      }
      f <- ars_fields(ln, ars_qc_labels)
      for (n in names(f)) hdr[[n]] <- f[[n]]
    }
  }
  if (!length(rows)) return(tibble::tibble())
  out <- dplyr::bind_rows(lapply(rows, tibble::as_tibble_row))
  out |>
    dplyr::mutate(
      method_id = ars_method_id(analysis),
      prep_date = as.Date(ars_datetime(prep_text, "QC Prep Date"))
    ) |>
    dplyr::select(-analysis, -prep_text) |>
    dplyr::relocate(prep_batch, qc_type, method_id, analyte, parent_sample, prep_date, units,
                    .after = source_page)
}
`%||%` <- function(a, b) if (is.null(a)) b else a
# ---- case narrative and sample receipt -------------------------------------------------
ars_receipt_datetime <- function(date_txt, time_txt) {
  lubridate::parse_date_time(paste(date_txt, time_txt), c("mdY IMSp", "mdY IMp", "mdY HM"),
                             tz = "UTC", quiet = TRUE)
}
# Case narrative text plus the receipt events it states (the lab that received the samples,
# and any subcontract lab). The narrative is free text: any sentence that mentions a
# subcontract or an arrival and does not parse stops the run.
ars_narrative <- function(lines_by_page, file, cover) {
  first_line <- vapply(lines_by_page, \(l) {
    l <- trimws(l); l <- l[nzchar(l)]
    if (length(l)) l[1] else ""
  }, "")
  start <- which(first_line == "Case Narrative")[1]
  if (is.na(start)) stop(file, ": no 'Case Narrative' page found.", call. = FALSE)
  after <- which(grepl("^Detailed Analytical Report", first_line) & seq_along(first_line) > start)[1]
  end <- if (is.na(after)) length(lines_by_page) else after - 1
  nar_lines <- trimws(unlist(lines_by_page[start:end]))
  narrative_text <- trimws(gsub("\n{3,}", "\n\n", paste(nar_lines, collapse = "\n")))
  s <- grep("^SAMPLE RECEIPT", nar_lines)[1]
  e <- grep("^REVIEW FOR COMPLIANCE", nar_lines)[1]
  if (is.na(s) || is.na(e) || e <= s) stop(file, ": no SAMPLE RECEIPT section in the case narrative.", call. = FALSE)
  sec <- gsub("\\s+", " ", trimws(paste(nar_lines[(s + 1):(e - 1)], collapse = " ")))
  sentences <- strsplit(sec, "(?<=\\.)\\s+(?=[A-Z])", perl = TRUE)[[1]]
  dt <- "(\\d{1,2}/\\d{1,2}/\\d{4})\\s+(\\d{1,2}:\\d{2}(?::\\d{2})?\\s*[AP]M)"
  d_only <- "(\\d{1,2}/\\d{1,2}/\\d{4})"
  temp_pat <- "temperatures? of\\s+(-?[\\d.]+)(?:\\s*(?:°|deg\\.?)?\\s*C)?(?:\\s*(?:and|,)\\s*(-?[\\d.]+)(?=\\s*(?:°|deg|C)))?"
  sub_pat <- paste0("subcontracted to (.+?) and arrived ", dt, "(?: and at a temperature of\\s+(-?[\\d.]+))?")
  # 2014 wording: transferred to a second lab, received there at a temperature, date only
  xfer_pat <- paste0("transferred\\b.*?\\bto\\s+(.+?),.*?where they were received\\b.*?temperature of\\s+(-?[\\d.]+).*?\\bon\\s+", d_only)
  prim_pat <- paste0("\\breceived\\s+(?:on\\s+)?", dt)
  lab_pat <- "\\sat\\s+(?!a temperature|temperatures?\\b)(.+?)\\s*\\.?$"
  events <- list(); cond <- NA_character_; loose_temp <- NA_real_
  temp_of <- function(snt) {
    mt <- regmatches(snt, regexec(temp_pat, snt, perl = TRUE))[[1]]
    if (!length(mt)) return(list(max = NA_real_, all = NA_character_))
    v <- as.numeric(c(mt[2], if (nzchar(mt[3])) mt[3]))
    list(max = max(v), all = if (length(v) > 1) paste(v, collapse = "; ") else NA_character_)
  }
  for (snt in sentences) {
    m <- regmatches(snt, regexec(sub_pat, snt, perl = TRUE))[[1]]
    if (length(m)) {
      events[[length(events) + 1]] <- list(role = "subcontract", lab = trimws(m[2]), date = m[3], time = m[4],
                                           temp = if (nzchar(m[5])) as.numeric(m[5]) else NA_real_, temp_all = NA_character_)
      next
    }
    m <- regmatches(snt, regexec(xfer_pat, snt, perl = TRUE))[[1]]
    if (length(m)) {
      events[[length(events) + 1]] <- list(role = "subcontract", lab = trimws(m[2]), date = m[4], time = NA_character_,
                                           temp = as.numeric(m[3]), temp_all = NA_character_)
      next
    }
    m <- regmatches(snt, regexec(prim_pat, snt, perl = TRUE))[[1]]
    if (length(m) && !grepl("subcontract|transferred", snt, ignore.case = TRUE)) {
      tp <- temp_of(snt)
      ml <- regmatches(snt, regexec(lab_pat, snt, perl = TRUE))[[1]]
      events[[length(events) + 1]] <- list(role = "primary", lab = if (length(ml)) trimws(ml[2]) else NA_character_,
                                           date = m[2], time = m[3], temp = tp$max, temp_all = tp$all)
      next
    }
    if (grepl("subcontract|transferred", snt, ignore.case = TRUE) || grepl("arrived.*\\d{1,2}/\\d{1,2}/\\d{4}", snt)) {
      stop(file, ": receipt sentence not understood: ", snt, call. = FALSE)
    }
    tp <- temp_of(snt)
    if (!is.na(tp$max) && is.na(loose_temp)) loose_temp <- tp$max
    if (grepl("condition", snt, ignore.case = TRUE)) cond <- trimws(snt)
  }
  roles <- vapply(events, `[[`, "", "role")
  if (sum(roles == "primary") != 1) stop(file, ": expected exactly one primary receipt statement, found ", sum(roles == "primary"), ".", call. = FALSE)
  rec <- dplyr::bind_rows(lapply(events, \(ev) {
    has_time <- !is.na(ev$time)
    when <- if (has_time) ars_receipt_datetime(ev$date, ev$time) else lubridate::mdy(ev$date, tz = "UTC")
    if (is.na(when)) stop(file, ": unparseable receipt date/time: ", ev$date, " ", ev$time, call. = FALSE)
    tibble::tibble(role = ev$role, receiving_lab = ev$lab, receipt_date_printed = as.Date(when),
                   receipt_time = if (has_time) format(when, "%H:%M:%S") else NA_character_,
                   temperature_c = ev$temp, temperature_all = ev$temp_all)
  }))
  p <- rec$role == "primary"
  if (is.na(rec$temperature_c[p])) rec$temperature_c[p] <- loose_temp
  cover_date <- suppressWarnings(as.Date(lubridate::mdy(cover$date_received)))
  rec$condition_text <- ifelse(p, cond, NA_character_)
  rec$cover_date_received <- ifelse(p, format(cover_date), NA_character_)
  rec$receipt_date <- rec$receipt_date_printed
  rec$date_check <- NA_character_
  if (is.na(cover_date)) {
    rec$date_check[p] <- "no cover-page Date Received"
  } else if (rec$receipt_date_printed[p] != cover_date) {
    rec$date_check[p] <- paste0("narrative date ", rec$receipt_date_printed[p],
                                " differs from cover-page Date Received; cover date used")
    rec$receipt_date[p] <- cover_date
  }
  rec <- dplyr::bind_cols(tibble::tibble(source_file = file, workorder = cover$workorder), rec)
  list(case_narrative = tibble::tibble(source_file = file, workorder = cover$workorder,
                                       narrative_text = narrative_text),
       sample_receipt = rec)
}
# ---- one PDF ---------------------------------------------------------------------
extract_ars_pdf <- function(path) {
  for (pkg in c("pdftools", "dplyr", "tibble", "lubridate")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Package '", pkg, "' is required.", call. = FALSE)
  }
  file <- basename(path)
  pages <- pdftools::pdf_text(path)
  if (!any(nzchar(trimws(pages)))) stop(file, " has no text layer (OCR would be needed).", call. = FALSE)
  lines_by_page <- lapply(pages, \(p) sub("\\s+$", "", strsplit(p, "\r?\n")[[1]]))
  cover <- ars_cover(lines_by_page)
  all_rows <- ars_parse_samples(lines_by_page, file)
  qc <- ars_parse_qc(lines_by_page, file)
  narrative <- ars_narrative(lines_by_page, file, cover)
  # Surrogate recoveries are lab QC: kept apart from analyte results
  surrogates <- all_rows[all_rows$row_type == "surrogate", ]
  all_rows <- all_rows[all_rows$row_type == "analyte", c(setdiff(names(all_rows), c("row_type", "pct_recovery", "lcl", "ucl")))]
  is_mb <- all_rows$report_section == "Method Blank Report"
  results <- all_rows[!is_mb, ]
  method_blanks <- all_rows[is_mb, ]
  by_sample <- results |>
    dplyr::group_by(source_file, workorder, lab_sample_base, client_sample_name) |>
    dplyr::summarise(n_blocks = dplyr::n_distinct(lab_sample_number),
                     n_analyte_rows = dplyr::n(),
                     methods = paste(sort(unique(method_id)), collapse = ", "),
                     .groups = "drop")
  listed <- cover$listed$lab_sample_base
  accounting <- tibble::tibble(
    source_file = file,
    workorder = cover$workorder,
    date_received = cover$date_received,
    n_listed_on_cover = length(listed),
    n_with_results = dplyr::n_distinct(results$lab_sample_base),
    listed_without_results = paste(setdiff(listed, results$lab_sample_base), collapse = ", "),
    results_not_listed = paste(setdiff(results$lab_sample_base, listed), collapse = ", "),
    n_analyte_rows = nrow(results),
    n_method_blank_rows = nrow(method_blanks),
    n_qc_rows = nrow(qc)
  )
  list(results = results, method_blanks = method_blanks, surrogates = surrogates, qc = qc,
       accounting = accounting, by_sample = by_sample, cover = cover,
       case_narrative = narrative$case_narrative, sample_receipt = narrative$sample_receipt)
}
# ---- a folder of PDFs ----------------------------------------------------------------
extract_ars_dir <- function(dir, pattern = "\\.pdf$") {
  files <- list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) stop("No PDFs found in ", dir, call. = FALSE)
  res <- lapply(files, extract_ars_pdf)
  out <- list(
    results = dplyr::bind_rows(lapply(res, `[[`, "results")),
    method_blanks = dplyr::bind_rows(lapply(res, `[[`, "method_blanks")),
    surrogates = dplyr::bind_rows(lapply(res, `[[`, "surrogates")),
    qc = dplyr::bind_rows(lapply(res, `[[`, "qc")),
    accounting = dplyr::bind_rows(lapply(res, `[[`, "accounting")),
    by_sample = dplyr::bind_rows(lapply(res, `[[`, "by_sample")),
    case_narrative = dplyr::bind_rows(lapply(res, `[[`, "case_narrative")),
    sample_receipt = dplyr::bind_rows(lapply(res, `[[`, "sample_receipt"))
  )
  a <- out$accounting
  if (any(nzchar(a$listed_without_results) | nzchar(a$results_not_listed))) {
    warning("Cover-page sample list and parsed results disagree; see $accounting.", call. = FALSE)
  }
  out
}
