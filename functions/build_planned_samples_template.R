# build_planned_samples_template.R
#
# Developer tool (not part of the pipeline run). Writes the blank planned-samples
# template that the user fills in each year before the QA/QC run:
#   reference/analyte_groups.csv   analyte group -> analyte -> fraction map (editable)
#   year_files/planned_samples.xlsx sheets: README, analyte_rules, qc_samples, site_list
#
# The workbook holds the standard plan pre-filled from the QAPP and the field manual;
# items that vary between years start blank with needs_confirm = "Y" (analyte_rules) or
# a blank confirm cell (qc_samples). Sampling dates live in config.yml, not here.
#
# Usage: source(here::here("functions/build_planned_samples_template.R"))
#        build_planned_samples_template()   # refuses to overwrite a filled-in workbook
#
# Site rule semantics (shared with the expansion code that reads the workbook):
#   site_rule "all"   : every site in site_list
#   site_rule "range" : sites with rm_min <= river_mile <= rm_max (blank bound = no bound)
#   site_rule "list"  : only the river miles in add_rm
#   add_rm / exclude_rm (semicolon-separated river miles) are added to or removed from
#   the result for any site_rule.

build_planned_samples_template <- function(
    groups_path = here::here("reference/analyte_groups.csv"),
    sites_path  = here::here("reference/baseline_sites.csv"),
    out_path    = here::here("year_files/planned_samples.xlsx"),
    overwrite   = FALSE) {

  if (file.exists(out_path) && !overwrite) {
    stop("build_planned_samples_template: ", basename(out_path), " exists; it may hold a ",
         "year's answers. Use overwrite = TRUE to replace it.", call. = FALSE)
  }

  groups <- tibble::tribble(
    ~analyte_group,          ~analyte,                  ~sample_fraction, ~measured_by,
    "Dissolved metals",      "Arsenic",                 "Dissolved",      "SGS",
    "Dissolved metals",      "Cadmium",                 "Dissolved",      "SGS",
    "Dissolved metals",      "Chromium",                "Dissolved",      "SGS",
    "Dissolved metals",      "Copper",                  "Dissolved",      "SGS",
    "Dissolved metals",      "Lead",                    "Dissolved",      "SGS",
    "Dissolved metals",      "Zinc",                    "Dissolved",      "SGS",
    "Total metals",          "Calcium",                 "Unfiltered",     "SGS",
    "Total metals",          "Magnesium",               "Unfiltered",     "SGS",
    "Total metals",          "Iron",                    "Unfiltered",     "SGS",
    "Total metals",          "Copper",                  "Unfiltered",     "SGS",
    "Total metals",          "Zinc",                    "Unfiltered",     "SGS",
    "Nutrients",             "Total Nitrate/Nitrite-N", NA,               "SGS",
    "Nutrients",             "Total Phosphorus",        NA,               "SGS",
    "Hydrocarbons (BTEX)",   "Benzene",                 NA,               "SGS",
    "Hydrocarbons (BTEX)",   "Toluene",                 NA,               "SGS",
    "Hydrocarbons (BTEX)",   "Ethylbenzene",            NA,               "SGS",
    "Hydrocarbons (BTEX)",   "o-Xylene",                NA,               "SGS",
    "Hydrocarbons (BTEX)",   "P & M -Xylene",           NA,               "SGS",
    "Hydrocarbons (BTEX)",   "Xylenes (total)",         NA,               "SGS",
    "Fecal coliform",        "Fecal Coliform",          NA,               "SWWTP",
    "TSS",                   "Total suspended solids",  NA,               "SWWTP",
    "Field parameters",      "Water Temperature",       NA,               "YSI ProQuatro",
    "Field parameters",      "Specific Conductance",    NA,               "YSI ProQuatro",
    "Field parameters",      "Dissolved Oxygen",        NA,               "YSI ProQuatro",
    "Field parameters",      "pH",                      NA,               "YSI ProQuatro",
    "Turbidity",             "Turbidity",               NA,               "Hach turbidimeter"
  )

  known <- utils::read.csv(here::here("reference/wqx_templates/analytes_list_manual_edit.csv"))$analyte
  unknown <- setdiff(groups$analyte, known)
  if (length(unknown)) {
    stop("Analytes not in analytes_list_manual_edit.csv: ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  utils::write.csv(groups, groups_path, row.names = FALSE, na = "")

  # Standard plan. Blank spring/summer with needs_confirm = "Y" means "ask each year".
  rules <- groups |>
    dplyr::mutate(
      spring = "Y", summer = "Y",
      site_rule = "all", rm_min = NA_real_, rm_max = NA_real_,
      add_rm = NA_character_, exclude_rm = NA_character_,
      needs_confirm = NA_character_, notes = NA_character_
    )

  is_grp <- function(g) rules$analyte_group == g
  rules$site_rule[is_grp("Dissolved metals")] <- "range"
  rules$rm_max[is_grp("Dissolved metals")]    <- 30
  rules$notes[is_grp("Dissolved metals")] <-
    "QAPP v3: dissolved metals at RM 30 and downstream (smaller river miles)."

  extra_total <- is_grp("Total metals") & rules$analyte %in% c("Iron", "Copper", "Zinc")
  rules$spring[extra_total] <- NA_character_
  rules$summer[extra_total] <- NA_character_
  rules$needs_confirm[extra_total] <- "Y"
  rules$notes[extra_total] <-
    "Total Ca and Mg are standard; other total metals vary by year. Enter Y or N for each season."

  btex <- is_grp("Hydrocarbons (BTEX)")
  rules$spring[btex]    <- "N"
  rules$site_rule[btex] <- "list"
  rules$add_rm[btex]    <- "1.5;6.5;40;43"
  rules$notes[btex]     <- "Summer only; sites per the QAPP v3 field manual."

  rules <- rules |>
    dplyr::select(analyte_group, analyte, sample_fraction, spring, summer, site_rule,
                  rm_min, rm_max, add_rm, exclude_rm, needs_confirm, notes)

  qc <- tibble::tribble(
    ~season,  ~sample_type,       ~river_mile, ~analyte_group,                    ~confirm, ~notes,
    "spring", "Field Duplicate",  NA,          "All analytes",                    NA,       "Enter the river mile.",
    "spring", "Field Duplicate",  NA,          "All analytes",                    NA,       "Enter the river mile.",
    "spring", "Field Blank",      NA,          "Dissolved metals; Total metals",  NA,       "Enter the river mile (RM 30 or downstream).",
    "spring", "Field Blank",      NA,          "Dissolved metals; Total metals",  NA,       "Enter the river mile (RM 30 or downstream).",
    "summer", "Field Duplicate",  NA,          "All analytes",                    NA,       "Enter the river mile.",
    "summer", "Field Duplicate",  NA,          "All analytes",                    NA,       "Enter the river mile.",
    "summer", "Field Blank",      NA,          "Dissolved metals; Total metals",  NA,       "Enter the river mile (RM 30 or downstream).",
    "summer", "Field Blank",      NA,          "Dissolved metals; Total metals",  NA,       "Enter the river mile (RM 30 or downstream).",
    "summer", "Trip Blank",       NA,          "Hydrocarbons (BTEX)",             NA,       "One per cooler. Enter the river miles the cooler covers, separated by semicolons.",
    "summer", "Trip Blank",       NA,          "Hydrocarbons (BTEX)",             NA,       "One per cooler. Add or delete rows to match the number of coolers."
  )

  sites <- utils::read.csv(sites_path, check.names = FALSE, stringsAsFactors = FALSE)
  site_list <- tibble::tibble(
    river_mile             = as.numeric(sites$river_mile),
    monitoring_location_id = sub("^KENAI_WQX-", "", sites$MonitoringLocationIdentifier),
    location_name          = sites$location_name
  ) |>
    dplyr::arrange(river_mile)

  readme <- tibble::tibble(text = c(
    "PLANNED SAMPLES: fill in before running qaqc.qmd",
    "",
    "The QA/QC run stops until every required cell below is answered. Q3, Q23, Q25 and Q26 compare what was collected against this plan.",
    "Sampling dates are not entered here; they come from config.yml.",
    "",
    "analyte_rules: one row per analyte. Standard rows are pre-filled from QAPP v3 and the field manual; change them only if the plan differed.",
    "  spring, summer: Y if the analyte was planned in that event, N if not.",
    "  site_rule: all = every site; range = sites from rm_min to rm_max (blank bound = no bound); list = only the river miles in add_rm.",
    "  add_rm, exclude_rm: river miles (semicolon-separated) added to or removed from the sites chosen by site_rule.",
    "  needs_confirm = Y marks rows that vary by year: enter Y or N in spring and summer. A blank spring or summer cell stops the run.",
    "  sample_fraction: Dissolved or Unfiltered for metals; blank where the fraction does not distinguish rows.",
    "  Rows for analytes not planned this year: set spring and summer to N. To plan an analyte not listed, add it to reference/analyte_groups.csv and re-run build_planned_samples_template().",
    "",
    "qc_samples: one row per planned field QC sample (duplicates, field blanks, trip blanks).",
    "  river_mile: the site where the sample was planned (duplicate and field blank sites rotate each event). Trip blanks may list several river miles separated by semicolons.",
    "  Defaults: 2 duplicates and 2 field blanks per event; trip blanks one per cooler, summer only. Add or delete rows to match the plan.",
    "  confirm: type Y once the row matches the plan. A blank river_mile or confirm cell stops the run.",
    "",
    "site_list: the project sites (read-only reference).",
    "Planned field QC samples count toward expected results; laboratory QC (blanks, LCS, matrix spikes) does not."
  ))

  wb <- openxlsx::createWorkbook()
  head_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9E1F2", border = "bottom")
  input_style <- openxlsx::createStyle(fgFill = "#FFF2CC")
  add_sheet <- function(name, x, widths) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, x, headerStyle = head_style, keepNA = FALSE)
    openxlsx::setColWidths(wb, name, cols = seq_along(widths), widths = widths)
    openxlsx::freezePane(wb, name, firstRow = TRUE)
  }
  openxlsx::addWorksheet(wb, "README")
  openxlsx::writeData(wb, "README", readme, colNames = FALSE)
  openxlsx::setColWidths(wb, "README", cols = 1, widths = 150)
  add_sheet("analyte_rules", rules, c(22, 26, 16, 9, 9, 11, 9, 9, 16, 12, 14, 70))
  add_sheet("qc_samples", qc, c(9, 18, 14, 34, 10, 80))
  add_sheet("site_list", site_list, c(12, 24, 30))

  openxlsx::dataValidation(wb, "analyte_rules", cols = 4:5, rows = 2:(nrow(rules) + 1),
                           type = "list", value = '"Y,N"')
  openxlsx::dataValidation(wb, "analyte_rules", cols = 6, rows = 2:(nrow(rules) + 1),
                           type = "list", value = '"all,range,list"')
  openxlsx::dataValidation(wb, "qc_samples", cols = 2, rows = 2:200,
                           type = "list", value = '"Field Duplicate,Field Blank,Trip Blank"')
  openxlsx::dataValidation(wb, "qc_samples", cols = 5, rows = 2:200, type = "list", value = '"Y"')

  # Yellow = cells the user must answer
  flag_rows <- which(rules$needs_confirm %in% "Y") + 1
  openxlsx::addStyle(wb, "analyte_rules", input_style, rows = flag_rows, cols = 4:5, gridExpand = TRUE)
  openxlsx::addStyle(wb, "qc_samples", input_style, rows = 2:(nrow(qc) + 1), cols = c(3, 5),
                     gridExpand = TRUE)

  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  invisible(list(analyte_rules = rules, qc_samples = qc, site_list = site_list))
}
