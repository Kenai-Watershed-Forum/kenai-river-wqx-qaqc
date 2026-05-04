# REFERENCE FILE -- 2021 Worked Example
#
# This is the 2021 Fecal Coliform ingest script, preserved as a reference for adapting
# Part A (Fecal Coliform section) of pipeline_template.qmd to new data years.
#
# Do NOT source this file directly from any year's QMD. Instead, copy and adapt
# the relevant parser block(s) into Part A of your year's QMD.
#
# Key things to verify when adapting for a new year:
#   - spring_fc_lab and summer_fc_lab variables (set in Year Configuration)
#   - For 2022+: both seasons use "SWWTP" (Taurianen closed after 2021).
#     Taurianen was also used in some years prior to 2021 -- the parser below
#     applies to those years as well if adapting the pipeline retroactively.
#   - SWWTP FC file format: confirm skip = 11 still applies; run names() after read-in
#   - SWWTP site name matching table: update if site names changed
#   - FC receipt time (spring_fc_rec_time): read from chain of custody each year
#
# Original source: functions/appendix_a_scripts/ingest_fc.R (report repo)
# Canonical reference as of: April 2026

# ingest_fc.R
#
# Reads spring and summer fecal coliform data and binds them into `dat`.
# Contains two format-specific parsing blocks (SWWTP and Taurianen), selected
# by the `spring_fc_lab` and `summer_fc_lab` config variables.
#
# For 2021: spring_fc_lab = "SWWTP", summer_fc_lab = "Taurianen"
# For some years prior to 2021: Taurianen was also used for summer FC.
#   Use the Taurianen parser block for those years as well.
# For 2022+: both = "SWWTP" (Taurianen closed after 2021)
# If a new lab is used in a future year, add a new format block below.
#
# Requires in global env (set in appendix_a.qmd year-config block):
#   dat, spring_fc_path, summer_fc_path, spring_fc_lab, summer_fc_lab,
#   spring_sample_date, summer_sample_date, spring_rec_date, summer_rec_date,
#   spring_fc_analysis_date, summer_fc_analysis_date, output_qaqc_dir
#
# Produces in global env:
#   dat  (with FC rows added)
#
# Also writes:
#   {output_qaqc_dir}/spring_fc_qaqc_dat.csv
#   {output_qaqc_dir}/summer_fc_qaqc_dat.csv


############################################################################################################
##################################### Read in and Clean FC Data ############################################
############################################################################################################

# ── Spring FC ──────────────────────────────────────────────────────────────────
if (spring_fc_lab == "SWWTP") {

  ########################### SWWTP FC Format Parser ##################################################
  # Used for: 2021 spring and all future years (SWWTP is now the only FC lab).
  # File: .xls, 11 header rows to skip.

  swwtp_spring_fc <- read_excel(spring_fc_path, skip = 11) %>%
    clean_names() %>%

    # move info about duplicate sample and/or sample blank status into separate column
    mutate(sample_type = case_when(
      grepl("BLANK", sample_location_rm) ~ "MB",       # method blank
      grepl("POSITIVE", sample_location_rm) ~ "LCS")) %>%  # laboratory control sample
    # assign all other samples as "PS" (project sample)
    mutate_at(vars(sample_type), ~replace_na(., "PS")) %>%

    # field dup designation
    mutate(sample_condition = case_when(
      grepl("DUP", sample_location_rm) ~ "Field Duplicate")) %>%
    # remove "BLANK" and "POSITIVE" designation from sample_location column
    mutate(sample_location_rm = (str_replace(sample_location_rm, "BLANK|POSITIVE", "")))

  # remove "DUP" from site name column and trim white spaces
  swwtp_spring_fc %<>%
    mutate(sample_location_rm = str_remove(sample_location_rm, "DUP")) %>%
    mutate(sample_location_rm = str_trim(sample_location_rm, "right"))

  # join site names matching table
  swwtp_spring_fc_site_matching <- read_excel(
    "other/input/wqx_templates/swwtp_site_names_matching_table_manual_edit.xlsx")
  swwtp_spring_fc %<>%
    full_join(swwtp_spring_fc_site_matching) %>%
    select(-sample_location_rm)
  rm(swwtp_spring_fc_site_matching)

  # fix lab analysis times and dates
  spring_fc_rec_time <- "13:31:00"
  swwtp_spring_fc %<>%
    mutate(analysis_time_in = as_hms(time_in),
           analysis_date_in = mdy(spring_sample_date),
           analysis_time_out = as_hms(time_out),
           # see spring FC file for out analysis date (typically day after sampling)
           analysis_date_out = spring_fc_analysis_date) %>%
    select(-time_in, -time_out) %>%
    transform(time_sampled = as_hms(time_sampled)) %>%
    mutate(time_sampled = as_hms(time_sampled),
           sample_date = mdy(spring_sample_date))

  # assign receipt date/time from chain of custody
  swwtp_spring_fc %<>%
    mutate(rec_date = ymd(spring_rec_date),
           rec_time = as_hms(spring_fc_rec_time))

  # rename columns and add WQX-required metadata
  swwtp_spring_fc %<>%
    rename(lab_sample = dish_number,
           result = colony_count_100m_l,
           collect_time = time_sampled,
           run_time = analysis_time_in,
           run_date = analysis_date_in,
           collect_date = sample_date) %>%
    mutate(note = paste0("Lab analysis volume = ", ml, " mL"),
           matrix = "Water (Surface, Eff., Ground)",
           analytical_method = "9222 D ~ Membrane filtration test for fecal coliforms",
           analyte = "Fecal Coliform",
           units = "cfu/100ml",
           loq = 1.0,   # reporting limit from 2019 QAPP, pg 17
           lab_name = "Soldotna Wastewater Treatment Plant, Soldotna, Alaska",
           epa_analysis_id = "9222D",
           context_code = "APHA",
           analyst = "AW") %>%
    clean_names() %>%
    select(-ml, -colony_count) %>%
    transform(lab_sample = as.character(lab_sample),
              result = as.double(result)) %>%
    # apply correction to "TNTC" (Too Numerous To Count) result
    mutate(note = case_when(
      lab_sample == "30" ~ paste("Lab analysis volume = 0.5 mL, result = TNTC"),
      TRUE ~ note))

  # assign resultflag: "=" if result > 1.0 cfu, "U" if result < 1.0 cfu
  swwtp_spring_fc %<>%
    mutate(resultflag = case_when(
      result < 1 ~ "U",
      TRUE ~ "="))

  # modify non-detect values from "0" to "NA"
  swwtp_spring_fc %<>%
    mutate(result1 = na_if(result, 0)) %>%
    select(-result) %>%
    rename(result = result1)

  # segregate lab QA/QC results and write to CSV
  swwtp_spring_fc_qaqc <- swwtp_spring_fc %>%
    filter(sample_type %in% c("MB", "LCS"))
  write.csv(swwtp_spring_fc_qaqc,
            file.path(output_qaqc_dir, "spring_fc_qaqc_dat.csv"),
            row.names = FALSE)

  swwtp_spring_fc %<>%
    filter(!sample_type %in% c("MB", "LCS"))

  # bind spring FC into overall dataset
  dat <- bind_rows(dat, swwtp_spring_fc) %>%
    select(-location)
  rm(swwtp_spring_fc, swwtp_spring_fc_qaqc)

} else {
  stop(paste("Unknown spring_fc_lab:", spring_fc_lab,
             "-- add a format parser block to ingest_fc.R"))
}

# ── Summer FC ──────────────────────────────────────────────────────────────────
if (summer_fc_lab == "Taurianen") {

  ########################### Taurianen FC Format Parser ##############################################
  # Used for: 2021 summer and some prior years. Taurianen closed after 2021.
  # File: .xlsx, 3 header rows to skip.

  summer_fc_rec_time <- "13:37:00"

  taur_summer_fc <- read_excel(summer_fc_path, skip = 3) %>%
    clean_names() %>%
    select(-qc1, -data_entry, -qc2) %>%

    # move info about duplicate sample and/or sample blank status into separate column
    mutate(sample_condition = case_when(
      grepl("DUP", sample_location) ~ "Field Duplicate")) %>%
    # remove "DUP" designation from sample_location column
    mutate(sample_location = (str_replace(sample_location, "_DUP", ""))) %>%
    # trim white spaces
    mutate(sample_location = str_trim(sample_location, "right")) %>%

    # add known date/time info
    mutate(collect_date = mdy(summer_sample_date),
           run_date = mdy(summer_sample_date),
           run_time = as_hms(time_relinquished),
           analysis_date_out = summer_fc_analysis_date,
           analysis_time_out = as_hms(time_tested),
           rec_date = ymd(summer_rec_date),
           rec_time = as_hms(summer_fc_rec_time),
           .keep = "unused") %>%
    select(-date_of_testing, -neg_pos) %>%
    transform(time_sampled = as_hms(time_sampled)) %>%

    mutate(lab_name = "Taurianen Engineering and Testing, Soldotna, Alaska") %>%
    rename(sample = sample_location,
           collect_time = time_sampled)

  # NOTE: for Taurianen QA/QC practices, see email at
  # "other/documents/references/Taurianen QA Technique (Email march 2022).docx"

  # generate and join site name matching table
  taur_fc_sites <- data.frame(unique(taur_summer_fc$sample)) %>%
    rename(sample = unique.taur_summer_fc.sample.)
  write.xlsx(taur_fc_sites, "other/input/wqx_templates/taurianen_site_names_matching_table.xlsx")
  taur_fc_sites <- read_excel("other/input/wqx_templates/taurianen_site_names_matching_table_manual_edit.xlsx")
  taur_summer_fc <- left_join(taur_summer_fc, taur_fc_sites, by = "sample")

  # add/rename columns to match SWWTP dataframe structure
  taur_summer_fc %<>%
    clean_names() %>%
    select(-direct_count) %>%
    rename(result = number_of_colonies) %>%
    mutate(note = "",
           matrix = "Water",
           analytical_method = "9222 D ~ Membrane filtration test for fecal coliforms",
           analyte = "Fecal Coliform",
           units = "cfu/100ml",
           loq = 1,
           epa_analysis_id = "9222D",
           context_code = "APHA") %>%
    transform(result = as.double(result))

  # assign sample_type
  taur_summer_fc %<>%
    mutate(sample_type = case_when(
      sample_condition == "Lab Blank" ~ "MB",
      sample_condition == "Positive Control" ~ "LCS",
      TRUE ~ "PS"))

  # assign resultflag
  taur_summer_fc %<>%
    mutate(resultflag = case_when(
      result < 1 ~ "U",
      TRUE ~ "="))

  # modify non-detect values from "0" to "NA"
  taur_summer_fc %<>%
    mutate(result1 = na_if(result, 0)) %>%
    select(-result) %>%
    rename(result = result1)

  # segregate lab QA/QC results and write to CSV
  taur_summer_fc_qaqc <- taur_summer_fc %>%
    filter(sample_type %in% c("MB", "LCS"))
  write.csv(taur_summer_fc_qaqc,
            file.path(output_qaqc_dir, "summer_fc_qaqc_dat.csv"),
            row.names = FALSE)

  # bind summer FC into overall dataset
  dat <- bind_rows(dat, taur_summer_fc)
  rm(taur_summer_fc, taur_fc_sites, taur_summer_fc_qaqc)

} else if (summer_fc_lab == "SWWTP") {

  ########################### SWWTP Summer FC (2022+) #################################################
  # For 2022+: SWWTP processes both spring and summer FC.
  # File format is the same as spring SWWTP above.

  summer_fc_rec_time <- ""  # update from summer chain of custody each year

  swwtp_summer_fc <- read_excel(summer_fc_path, skip = 11) %>%
    clean_names() %>%
    mutate(sample_type = case_when(
      grepl("BLANK", sample_location_rm) ~ "MB",
      grepl("POSITIVE", sample_location_rm) ~ "LCS")) %>%
    mutate_at(vars(sample_type), ~replace_na(., "PS")) %>%
    mutate(sample_condition = case_when(
      grepl("DUP", sample_location_rm) ~ "Field Duplicate")) %>%
    mutate(sample_location_rm = str_replace(sample_location_rm, "BLANK|POSITIVE", "")) %>%
    mutate(sample_location_rm = str_remove(sample_location_rm, "DUP")) %>%
    mutate(sample_location_rm = str_trim(sample_location_rm, "right"))

  swwtp_summer_fc_site_matching <- read_excel(
    "other/input/wqx_templates/swwtp_site_names_matching_table_manual_edit.xlsx")
  swwtp_summer_fc %<>%
    full_join(swwtp_summer_fc_site_matching) %>%
    select(-sample_location_rm)
  rm(swwtp_summer_fc_site_matching)

  swwtp_summer_fc %<>%
    mutate(analysis_time_in = as_hms(time_in),
           analysis_date_in = mdy(summer_sample_date),
           analysis_time_out = as_hms(time_out),
           analysis_date_out = summer_fc_analysis_date) %>%
    select(-time_in, -time_out) %>%
    transform(time_sampled = as_hms(time_sampled)) %>%
    mutate(time_sampled = as_hms(time_sampled),
           sample_date = mdy(summer_sample_date)) %>%
    mutate(rec_date = ymd(summer_rec_date),
           rec_time = as_hms(summer_fc_rec_time)) %>%
    rename(lab_sample = dish_number,
           result = colony_count_100m_l,
           collect_time = time_sampled,
           run_time = analysis_time_in,
           run_date = analysis_date_in,
           collect_date = sample_date) %>%
    mutate(note = paste0("Lab analysis volume = ", ml, " mL"),
           matrix = "Water (Surface, Eff., Ground)",
           analytical_method = "9222 D ~ Membrane filtration test for fecal coliforms",
           analyte = "Fecal Coliform",
           units = "cfu/100ml",
           loq = 1.0,
           lab_name = "Soldotna Wastewater Treatment Plant, Soldotna, Alaska",
           epa_analysis_id = "9222D",
           context_code = "APHA",
           analyst = "AW") %>%
    clean_names() %>%
    select(-ml, -colony_count) %>%
    transform(lab_sample = as.character(lab_sample),
              result = as.double(result)) %>%
    mutate(resultflag = case_when(result < 1 ~ "U", TRUE ~ "=")) %>%
    mutate(result1 = na_if(result, 0)) %>%
    select(-result) %>%
    rename(result = result1)

  swwtp_summer_fc_qaqc <- swwtp_summer_fc %>%
    filter(sample_type %in% c("MB", "LCS"))
  write.csv(swwtp_summer_fc_qaqc,
            file.path(output_qaqc_dir, "summer_fc_qaqc_dat.csv"),
            row.names = FALSE)
  swwtp_summer_fc %<>% filter(!sample_type %in% c("MB", "LCS"))

  dat <- bind_rows(dat, swwtp_summer_fc) %>% select(-location)
  rm(swwtp_summer_fc, swwtp_summer_fc_qaqc)

} else {
  stop(paste("Unknown summer_fc_lab:", summer_fc_lab,
             "-- add a format parser block to ingest_fc.R"))
}
