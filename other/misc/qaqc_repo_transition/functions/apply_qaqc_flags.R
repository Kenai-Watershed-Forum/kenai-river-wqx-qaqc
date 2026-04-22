# apply_qaqc_flags.R
#
# Reads the provisional WQX intermediate CSV, joins the manually-determined flag
# decisions, writes the flagged export CSV, and leaves export_dat in the global
# environment for downstream QA/QC calculations (Q25 CMA, Q26 CMB, etc.).
#
# This script must run before Q25 and Q26, which both read from cfg$flagged_export_path.
# The flagging chunk is placed at Q25 (before completeness measures) so that CMA
# and CMB calculations read from a freshly-written file in the same render pass.
#
# Requires in global env:
#   cfg -- list with keys:
#     cfg$wqx_intermediate_path  (provisional WQX CSV written by format_wqx.R)
#     cfg$flag_decisions_path    ({year}_data_flag_decisions.csv -- manually created)
#     cfg$flagged_export_path    (output: flagged export CSV)
#
# Produces in global env:
#   export_dat  (flagged, ready for downstream QA/QC chunks and generate_cdx_export.R)
#
# Also writes:
#   cfg$flagged_export_path  (e.g., other/output/wqx_formatted/intermediate/{year}_export_data_flagged.csv)


# Read in data formatted for export
export_dat <- read.csv(cfg$wqx_intermediate_path) %>%
  clean_names() %>%
  transform(activity_start_date = ymd(activity_start_date))

# Apply manually-chosen data flags.
# The flag_decisions CSV contains one row per flagged result, with columns:
#   activity_start_date, characteristic_name (or other join key), flag = "Y", notes
# Rows absent from flag_decisions default to flag = "N" (Accepted).
flag_decisions <- read.csv(cfg$flag_decisions_path) %>%
  select(-notes) %>%
  transform(activity_start_date = mdy(activity_start_date))

export_dat <- full_join(export_dat, flag_decisions) %>%
  mutate(flag = case_when(
    is.na(flag) ~ "N",
    TRUE ~ "Y"
  )) %>%
  filter(!is.na(activity_id))

write.csv(export_dat, cfg$flagged_export_path, row.names = FALSE)
