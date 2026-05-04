# REFERENCE FILE -- 2021 Worked Example
#
# This is the 2021 TSS ingest script, preserved as a reference for adapting
# Part A (Total Suspended Solids section) of pipeline_template.qmd to new data years.
#
# Do NOT source this file directly from any year's QMD. Instead, copy and adapt
# the relevant code into Part A of your year's QMD.
#
# Key things to verify when adapting for a new year:
#   - Sheet name: "Updated_Formatting" must exist in both XLSX files; confirm with excel_sheets()
#   - skip = 1: verify the header row count hasn't changed
#   - Column names: run names() after read-in; SWWTP TSS format has been relatively stable
#   - Lab QA gap: SWWTP did not report required lab QA in 2021 and 2022; check for current year
#     and reinstate the commented-out QC export block if lab QA is now reported
#   - rec_date time: "14:00:00" is a reasonable default; update from the chain of custody
#
# Original source: functions/appendix_a_scripts/ingest_tss.R (report repo)
# Canonical reference as of: April 2026

# ingest_tss.R
#
# Reads SWWTP spring and summer TSS XLSX files, normalizes columns, joins
# site names, assigns result flags, and binds TSS rows into `dat`.
#
# Known QA gap: SWWTP did not report required lab QA results for TSS in 2021
# and 2022 (no lab blank, lab duplicate, or external QC check sample). The
# commented-out write block below documents where that export would go if/when
# lab QA is reinstated. Verify for each year whether QA data was provided.
# See QAPP for required QA measurements.
#
# Requires in global env (set in appendix_a.qmd year-config block):
#   dat, spring_tss_path, summer_tss_path, spring_rec_date, summer_rec_date
#
# Produces in global env:
#   dat  (with TSS rows added)


# SWWTP TSS data
## Reformat TSS data to match WQX template

# read in spring and summer TSS files
swwtp_tss_spring <- read_excel(spring_tss_path, skip = 1, sheet = "Updated_Formatting") %>%
  clean_names() %>%
  transform(date_of_analysis = anydate(date_of_analysis)) %>%
  # add receipt date/time from lab COC (14:00 is a reasonable default; update from COC)
  mutate(rec_date = ymd_hms(paste0(spring_rec_date, " 14:00:00")))

swwtp_tss_summer <- read_excel(summer_tss_path, skip = 1, sheet = "Updated_Formatting") %>%
  clean_names() %>%
  transform(sample_time = anytime(sample_time)) %>%
  mutate(rec_date = ymd_hms(paste0(summer_rec_date, " 14:00:00")))

# combine spring & summer
swwtp_tss <- bind_rows(swwtp_tss_spring, swwtp_tss_summer) %>%
  remove_empty()
rm(swwtp_tss_spring, swwtp_tss_summer)

# prepare and format to match larger dataset
swwtp_tss %<>%
  select(-qc1, -data_entry, -x8) %>%
  rename(analysis_time = time) %>%
  transform(sample_time = as_hms(sample_time),
            analysis_time = as_hms(analysis_time)) %>%

  # move duplicate/blank status into separate column
  mutate(sample_condition = case_when(
    grepl("DUP", sample_location) ~ "Field Duplicate")) %>%
  # remove "DUP" designation from locations column
  mutate(sample_location = str_replace(sample_location, "_DUP", "")) %>%
  # replace "O" with zeros in location column (common typo)
  mutate(sample_location = str_replace(sample_location, "RM_O", "RM_0")) %>%
  # add units
  mutate(units = "mg/l") %>%
  rename(result = s_s_mg_l) %>%
  transform(result = as.numeric(result)) %>%

  # EPA analysis metadata from AWQMS template
  mutate(epa_analysis_id = "2540-D",
         analytical_method = "SM21-2540-+D",
         context_code = "APHA",
         note = "") %>%

  # remove weight columns not needed for WQX export
  select(-dried_wt, -paper_wt, -tare_wt_kg, -ml) %>%

  # modify date/time formats
  mutate(collect_date = as.character(paste(field_sample_date, sample_time)),
         run_date_time = as.character(paste(date_of_analysis, analysis_time)),
         .keep = "unused") %>%
  mutate(collect_time = as_hms(as.POSIXct(collect_date))) %>%
  mutate(collect_date = date(as.POSIXct(collect_date)),
         run_time = as_hms(ymd_hms(run_date_time)),
         run_date = date(ymd_hms(run_date_time)),
         .keep = "unused") %>%

  rename(analyst = signature) %>%

  mutate(lab_sample = "",
         matrix = "Water",
         analyte = "Total suspended solids",
         loq = 1.0,    # reporting limit
         lod = 0.31,   # method detection limit
         lab_name = "Soldotna Wastewater Treatment Plant, Soldotna, Alaska")

# assign sample_type
swwtp_tss %<>%
  mutate(sample_type = case_when(
    sample_condition == "Lab Blank" ~ "MB",
    sample_condition == "Positive Control" ~ "LCS",
    TRUE ~ "PS"))

# join site names
swwtp_tss_sitenames <- data.frame(unique(swwtp_tss$sample_location))
unlink("other/input/wqx_templates/swwtp_tss_sitenames.csv")
write.csv(swwtp_tss_sitenames, "other/input/wqx_templates/swwtp_tss_sitenames.csv", row.names = FALSE)
swwtp_tss_sitenames <- read_excel("other/input/wqx_templates/swwtp_tss_site_names_matching_table_manual_edit.xlsx")
swwtp_tss <- left_join(swwtp_tss, swwtp_tss_sitenames) %>%
  clean_names() %>%
  rename(sample = sample_location)

# assign result flags
# J: below LOQ but above LOD (0.31); U: below LOD; =: detected
swwtp_tss %<>%
  mutate(resultflag = case_when(
    result < 1 & result > 0.31 ~ "J",
    result < 0.31 ~ "U",
    TRUE ~ "="))

# modify non-detect values from "0" to "NA"
swwtp_tss %<>%
  mutate(result1 = na_if(result, 0)) %>%
  select(-result) %>%
  rename(result = result1)

# ── Lab QA/QC export (currently disabled due to SWWTP reporting gap) ──────────
# SWWTP did not report required lab QA results for TSS in 2021 or 2022.
# QAPP requires: lab blank, lab duplicate sample, external QC check sample.
# Reinstate this block when SWWTP resumes QA reporting. See session_log.md.
#
# swwtp_tss_qaqc <- swwtp_tss %>%
#   filter(sample_type %in% c("MB","LCS"))
# write.csv(swwtp_tss_qaqc,
#            file.path(output_qaqc_dir, "tss_qaqc_dat.csv"), row.names = FALSE)
# rm(swwtp_tss_qaqc)

# bind TSS into overall dataset
dat <- bind_rows(dat, swwtp_tss)
rm(swwtp_tss_sitenames, swwtp_tss)
