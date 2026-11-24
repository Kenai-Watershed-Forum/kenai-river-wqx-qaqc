## ----setup, message=FALSE, warning=FALSE--------
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(here)

config <- yaml::read_yaml(here("config.yml"))
year <- config$year

# format_wqx.R expects the spring date as M/D/YYYY
spring_d <- ymd(config$sampling$spring_date)
spring_sample_date <- sprintf("%d/%d/%d", month(spring_d), day(spring_d), year(spring_d))

cfg <- list(
  year                  = year,
  templates_dir         = here("reference/wqx_templates"),
  wqx_template_file     = here("year_files", paste0("AWQMS_KWF_Baseline_", year, ".xlsx")),
  wqx_intermediate_path = here("output/intermediate", paste0(year, "_kwf_baseline_results_wqx.csv")),
  flagged_export_path   = here("output/intermediate", paste0(year, "_export_data_flagged.csv")),
  flag_decisions_path   = here("year_files", paste0(year, "_data_flag_decisions.csv")),
  # Moves into the template layout when the repo is blanked (Phase E)
  wqx_downloads_dir     = here("other/input/WQX_downloads"),
  spring_sample_date    = spring_sample_date,
  summer_sample_date    = config$sampling$summer_date
)

dir.create(here("output/intermediate"), recursive = TRUE, showWarnings = FALSE)


## ----pipeline, message=FALSE, warning=FALSE-----
fn_files <- c("resolve_rm_site", "numeric_utils", "clock_utils", "check_dat_contract",
              "ingest_sgs", "ingest_fc", "ingest_tss", "ingest_ysi", "combine_dat",
              "assess_lab_qc", "assign_qc_status")
for (f in fn_files) source(here("functions", paste0(f, ".R")))

res <- combine_dat(config)
lab_qc <- assess_lab_qc(res$lab_qc)
qc <- assign_qc_status(res$dat, res$sample_batches, lab_qc, config$qc$blank_multiple)
dat <- qc$dat
lab_qc_actions <- qc$lab_qc_actions

source(here("functions/format_wqx.R"))
source(here("functions/apply_qaqc_flags.R"))

# The CDX export needs station and project downloads from WQP; skip when absent
wqp_project_file <- file.path(cfg$wqx_downloads_dir, "project/project.csv")
if (file.exists(wqp_project_file)) {
  source(here("functions/generate_cdx_export.R"))
} else {
  message("generate_cdx_export skipped: WQP downloads not found in ", cfg$wqx_downloads_dir)
}

# Inputs for prelim_report.qmd
write.csv(export_dat, here("output/assessed_results.csv"), row.names = FALSE)
write.csv(lab_qc, here("output/lab_qc_assessed.csv"), row.names = FALSE)


## ----q01----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q02----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q03----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q04----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q05----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q06----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q07----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q08----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q09----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q10----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q11----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q12----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q13----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q14----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q15----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q16----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q17----------------------------------------
#| eval: false
# # AUTO: not yet ported from project_year/2025.qmd


## ----q18----------------------------------------
#| eval: false
# # AUTO: not yet ported from project_year/2025.qmd


## ----q19----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q20----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q21----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q22----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q23----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q24----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q25----------------------------------------
#| eval: false
# # AUTO: not yet ported from project_year/2025.qmd


## ----q26----------------------------------------
#| eval: false
# # AUTO: not yet ported from project_year/2025.qmd


## ----q27----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q28----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q29----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q30----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q31----------------------------------------
#| eval: false
# # MANUAL: not yet ported from project_year/2025.qmd


## ----q32----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q33----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q34----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q35----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q36----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q37----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q38----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q39----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q40----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q41----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----q42----------------------------------------
#| eval: false
# # MIXED: not yet ported from project_year/2025.qmd


## ----cdx-file-summary, message=FALSE------------
cdx_files <- here("output", c("results_activities.csv", "project.csv", "station.csv"))
tibble(file = basename(cdx_files),
       rows = map_int(cdx_files, \(f) if (file.exists(f)) nrow(read.csv(f)) else NA_integer_))

