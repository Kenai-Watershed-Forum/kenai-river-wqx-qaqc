# apply_qaqc_flags.R
#
# Reads the provisional WQX intermediate CSV, finishes the QC status (Q19 field duplicate
# failures and the manual decisions file, on top of the lab QC status already carried from
# assign_qc_status()), writes the flagged export CSV, and leaves export_dat and rpd_pairs in
# the global environment for downstream QA/QC calculations (Q19, Q25 CMA, Q26 CMB, etc.).
#
# This script must run before Q25 and Q26, which both read from cfg$flagged_export_path.
#
# Requires in global env:
#   cfg -- list with keys:
#     cfg$wqx_intermediate_path  (provisional WQX CSV written by format_wqx.R; includes
#                                 dat_row, qc_status and qc_fail_reason)
#     cfg$flag_decisions_path    ({year}_data_flag_decisions.csv, manually created; header only
#                                 is fine)
#     cfg$flagged_export_path    (output: flagged export CSV)
#   holding_times  (assess_holding_times() output; hold time failures are Rejected)
#   packages: dplyr, janitor, lubridate; functions/assess_rpd.R and functions/finalize_qc_status.R
#             are sourced here.
#
# Produces in global env:
#   export_dat  (snake_case columns plus qc_status, qc_fail_reason, flag; read by generate_cdx_export.R)
#   rpd_pairs   (assess_rpd() output: both guidance rules per duplicate pair, for Q19)
#   (rpd_pairs: pairs with needs_decision also stop the script until a Y/N row exists.)
#   dt_pairs    (assess_dissolved_total() output, for Q18). Stops if a pair exceeds tolerance
#               without a Y/N decision row in the flag decisions file.
#
# Also writes:
#   cfg$flagged_export_path  (e.g., output/{year}_export_data_flagged.csv)

source(here::here("functions/assess_rpd.R"))
source(here::here("functions/assess_dissolved_total.R"))
source(here::here("functions/finalize_qc_status.R"))

# Required, not optional: a silent skip would leave hold time failures Accepted
if (!exists("holding_times", inherits = TRUE)) {
  stop("apply_qaqc_flags: run assess_holding_times() on dat and assign the result to ",
       "holding_times first.", call. = FALSE)
}

# Intermediate CSV with the original WQX column names (assess_rpd() expects them)
wqx_export <- read.csv(cfg$wqx_intermediate_path, check.names = FALSE,
                       colClasses = c(`Monitoring Location ID` = "character"))
for (cn in c("qc_status", "qc_fail_reason")) {
  if (!cn %in% names(wqx_export)) {
    stop("apply_qaqc_flags: the intermediate CSV has no ", cn, " column. Run assign_qc_status() ",
         "on dat before format_wqx.R.", call. = FALSE)
  }
}

rpd_pairs <- assess_rpd(wqx_export)

export_dat <- wqx_export |>
  clean_names() |>
  transform(activity_start_date = ymd(activity_start_date)) |>
  mutate(monitoring_location_id = as.character(monitoring_location_id))

# Manual decisions: one row per flagged result with activity_start_date (M/D/YYYY),
# characteristic_name, monitoring_location_id, flag = "Y" and notes.
manual_flags <- NULL
if (file.exists(cfg$flag_decisions_path)) {
  manual_flags <- read.csv(cfg$flag_decisions_path,
                           colClasses = c(
                             activity_start_date    = "character",
                             characteristic_name    = "character",
                             monitoring_location_id = "character",
                             result_sample_fraction = "character",
                             flag                   = "character",
                             notes                  = "character"
                           )) |>
    transform(activity_start_date = mdy(activity_start_date))
} else {
  warning("apply_qaqc_flags: no manual decisions file at ", cfg$flag_decisions_path,
          "; continuing without manual flags.", call. = FALSE)
}

# Q18: dissolved > total beyond tolerance needs a recorded decision before anything is exported
dt_pairs <- assess_dissolved_total(export_dat)
check_dissolved_total_decisions(dt_pairs, manual_flags)

# Q19: a duplicate fail with one result below the LOQ likewise needs a recorded decision
check_rpd_decisions(rpd_pairs, manual_flags)

export_dat <- finalize_qc_status(export_dat, rpd_pairs, manual_flags, holding_times) |>
  filter(!is.na(activity_id))

write.csv(export_dat, cfg$flagged_export_path, row.names = FALSE)
