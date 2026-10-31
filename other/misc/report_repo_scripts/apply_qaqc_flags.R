# apply_qaqc_flags.R
#
# Reads the provisional WQX intermediate CSV, joins the manually-determined flag
# decisions, writes the flagged export CSV, and leaves export_dat in the global
# environment for downstream QA/QC calculations (Q25 CMA, Q26 CMB, etc.).
#
# This script must run before Q25 and Q26, which both read from flagged_export_path.
# The flagging chunk was placed at Q25 (before completeness measures) so that CMA
# and CMB calculations read from a freshly-written file in the same render pass.
#
# Requires in global env (set in appendix_a.qmd year-config block):
#   wqx_intermediate_path, flag_decisions_path, flagged_export_path
#
# Produces in global env:
#   export_dat  (flagged, ready for downstream QA/QC chunks)
#
# Also writes:
#   flagged_export_path  (e.g., other/output/wqx_formatted/intermediate/{year}_export_data_flagged.csv)


# read in data formatted for export
export_dat <- read.csv(wqx_intermediate_path) %>%
  clean_names() %>%
  transform(activity_start_date = ymd(activity_start_date))

# apply manually-chosen data flags based on discussion in question 19
# NOTE: the flag decisions CSV also has a column for summary rationale for each decision
flag_decisions <- read.csv(flag_decisions_path) %>%
  select(-notes) %>%
  transform(activity_start_date = mdy(activity_start_date))

export_dat <- full_join(export_dat, flag_decisions) %>%
  mutate(flag = case_when(
    is.na(flag) ~ "N",
    TRUE ~ "Y"
  )) %>%
  filter(!is.na(activity_id))

write.csv(export_dat, flagged_export_path, row.names = FALSE)
