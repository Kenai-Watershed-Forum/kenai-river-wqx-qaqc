# Readiness check for a v2 template workbook (extractor output or an upgraded hand-entered
# workbook) before it goes into the QA/QC pipeline. Tests structure and rules only, never counts.
#
# check_template_ready(path, sheet = "KWF_Baseline_EPA_Template", columns_path = <template_columns.csv>)
# Returns a tibble: check, severity ("error" or "warning"), n_rows, examples (sheet rows).
# Needs: readxl, dplyr, tibble.

check_template_ready <- function(path, sheet = "KWF_Baseline_EPA_Template", columns_path = NULL) {
  if (is.null(columns_path)) {
    columns_path <- file.path("functions", "pdf_extract", "template_columns.csv")
    if (requireNamespace("here", quietly = TRUE)) columns_path <- here::here(columns_path)
  }
  cols <- utils::read.csv(columns_path, stringsAsFactors = FALSE)[[1]]
  d <- readxl::read_excel(path, sheet = sheet, col_types = "text")

  out <- list()
  flag <- function(check, severity, idx, label = NULL) {
    if (length(idx) == 0) return(invisible())
    ex <- if (is.null(label)) paste(utils::head(idx + 1L, 5), collapse = ", ") else paste(utils::head(label, 5), collapse = ", ")
    out[[length(out) + 1]] <<- tibble::tibble(check = check, severity = severity, n_rows = length(idx), examples = ex)
  }
  has <- \(nm) nm %in% names(d)

  if (!identical(names(d), cols)) {
    miss <- setdiff(cols, names(d)); extra <- setdiff(names(d), cols)
    flag(paste0("columns differ from template_columns.csv; missing: ", paste(miss, collapse = ", "),
                "; extra: ", paste(extra, collapse = ", "),
                if (!length(miss) && !length(extra)) "order differs"), "error", 1L, "header")
    if (length(miss)) return(dplyr::bind_rows(out))
  }

  num <- \(x) suppressWarnings(as.numeric(x))
  # Rows with content: anything in Lab Sample Number or Characteristic Name
  used <- !is.na(d[["Lab Sample Number"]]) | !is.na(d[["Characteristic Name"]])
  flag("rows with no lab sample number and no characteristic (empty or stray rows)", "warning", which(!used & rowSums(!is.na(d)) > 0))
  field <- used & !is.na(d[["Activity Type"]])
  surr <- grepl("surr", d[["Characteristic Name"]], ignore.case = TRUE) & !is.na(d[["Characteristic Name"]])
  flag("surrogate rows on the template sheet (belong on lab_qc, never bound for WQX)", "warning", which(field & surr))
  flag("rows with a characteristic but no Activity Type", "error", which(used & is.na(d[["Activity Type"]]) & !is.na(d[["Characteristic Name"]])))

  req <- c("Lab Sample Number", "Characteristic Name", "Result Analytical Method ID", "Result Unit",
           "Laboratory Name", "Analysis Start Date", "Sample Receipt Date")
  for (cn in req) flag(paste0("missing ", cn), "error", which(field & is.na(d[[cn]]) & !(cn == "Result Unit" & surr)))

  # Result value must be numeric when present
  rv <- d[["Result Value"]]
  flag("Result Value is text (for example '<5' or 'ND')", "error", which(field & !is.na(rv) & is.na(num(rv))))

  # Non-detect convention: blank value, qualifier U, condition Not Detected go together
  nd_c <- d[["Result Detection Condition"]] %in% "Not Detected"
  nd_q <- d[["Result Qualifier"]] %in% "U"
  flag("non-detect convention broken: Not Detected, qualifier U and blank value must agree", "error",
       which(field & (nd_c != nd_q | (nd_c & !is.na(rv)) | (!nd_c & is.na(rv)))))
  flag("Result Qualifier not in {U, J, blank}", "error",
       which(field & !is.na(d[["Result Qualifier"]]) & !d[["Result Qualifier"]] %in% c("U", "J")))
  flag("Result Detection Condition not in {Not Detected, Present Below Quantification Limit, blank}", "error",
       which(field & !is.na(d[["Result Detection Condition"]]) &
               !d[["Result Detection Condition"]] %in% c("Not Detected", "Present Below Quantification Limit")))
  flag("non-detect without a detection limit (Limit 1)", "error", which(field & nd_c & is.na(num(d[["Result Detection Limit Value 1"]]))))
  flag("detection limit text or non-numeric", "error",
       which(field & !is.na(d[["Result Detection Limit Value 1"]]) & is.na(num(d[["Result Detection Limit Value 1"]]))))

  # Dates must be numeric and receipt must not follow analysis start
  rd <- num(d[["Sample Receipt Date"]]); ad <- num(d[["Analysis Start Date"]])
  flag("receipt or analysis date is not a date value", "error",
       which(field & ((!is.na(d[["Sample Receipt Date"]]) & is.na(rd)) | (!is.na(d[["Analysis Start Date"]]) & is.na(ad)))))
  flag("receipt date after analysis start date", "error", which(field & !is.na(rd) & !is.na(ad) & rd > ad))
  flag("receipt date missing but receipt time present", "error", which(field & is.na(d[["Sample Receipt Date"]]) & !is.na(d[["Sample Receipt Time"]])))
  flag("receipt time missing", "warning", which(field & is.na(d[["Sample Receipt Time"]])))

  # Convenience columns numeric
  for (cn in c("Spike", "Percent Recovered", "LCL", "UCL")) {
    flag(paste0(cn, " is not numeric"), "error", which(field & !is.na(d[[cn]]) & is.na(num(d[[cn]]))))
  }
  flag("Percent Recovered present without LCL and UCL", "warning",
       which(field & !is.na(d[["Percent Recovered"]]) & (is.na(d[["LCL"]]) | is.na(d[["UCL"]]))))

  # Items that are blank by design in extractor output but must be filled before upload
  flag("Monitoring Location ID blank (reconcile site names before CDX)", "warning", which(field & is.na(d[["Monitoring Location ID"]])))
  flag("Activity ID blank (built later)", "warning", which(field & is.na(d[["Activity ID"]])))
  flag("Result Sample Fraction blank (filled by lookup later)", "warning", which(field & is.na(d[["Result Sample Fraction"]])))
  flag("Result Status ID blank (decided per year)", "warning", which(field & is.na(d[["Result Status ID"]])))
  tz <- d[["Analysis Start Time Zone"]]
  flag("Analysis Start Time Zone is not AKST", "warning", which(field & !is.na(tz) & tz != "AKST"))
  flag("Analysis Start Time Zone missing", "warning", which(field & is.na(tz)))

  if (!length(out)) return(tibble::tibble(check = character(), severity = character(), n_rows = integer(), examples = character()))
  dplyr::bind_rows(out)
}
