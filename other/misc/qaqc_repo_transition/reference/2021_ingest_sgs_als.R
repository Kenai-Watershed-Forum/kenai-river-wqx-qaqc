# REFERENCE FILE -- 2021 Worked Example
#
# This is the 2021 SGS/ALS ingest script, preserved as a reference for adapting
# Part A (SGS/ALS Lab Results section) of pipeline_template.qmd to new data years.
#
# Do NOT source this file directly from any year's QMD. Instead, copy and adapt
# the relevant sections into Part A of your year's QMD.
#
# Key things to verify when adapting for a new year:
#   - SGS EDD column names (run `names(spring_sgs_raw)` after read-in)
#   - Whether ALS was used (ALS supplied dissolved Ca/Fe/Mg in 2021; confirm for current year)
#   - Unit labels for dissolved Ca/Fe/Mg (see unit correction block below -- was a 2021 issue)
#   - Trip blank crew CSV: create other/input/wqx_templates/trip_blank_crews_{year}.csv
#   - Analysis code matching table: confirm methods match current year's QAPP
#
# Original source: functions/appendix_a_scripts/ingest_sgs_als.R (report repo)
# Canonical reference as of: April 2026

# ingest_sgs_als.R
#
# Reads SGS EDD and ALS CSV files, binds them, applies unit correction for
# dissolved Ca/Fe/Mg, normalizes column names, standardizes sample/site name
# strings, joins the site-name and analytical-method matching tables, segregates
# lab QA/QC rows, and exports a lab QA/QC intermediate CSV.
#
# Requires in global env (set in appendix_a.qmd year-config block):
#   year, spring_data_dir, summer_data_dir, output_qaqc_dir
#
# Produces in global env:
#   dat  (SGS + ALS field results, site-name-joined, ready for FC/TSS bind)
#
# Also writes:
#   other/input/wqx_templates/sample_type_abbreviations.xlsx
#   {output_qaqc_dir}/sgs_als_qaqc_dat.csv


################################################################################################################
######################################### Read in and Clean SGS/ALS Data ######################################
################################################################################################################


############################ Part A: SGS Data Read In #############################

## Reformat SGS data downloaded from their server client (SGS Engage, full EDD files) to match WQX template

# read in
spring_sgs_raw <- read.csv(file.path(spring_data_dir, "SGS", paste0("spring_", year, "_sgs_batch_info.csv")))
summer_sgs_raw <- read.csv(file.path(summer_data_dir, "SGS", paste0("summer_", year, "_sgs_batch_info.csv")))

# clean up and retain only useful columns
sgs_raw <- bind_rows(spring_sgs_raw, summer_sgs_raw) %>%
  clean_names() %>%
  remove_empty() %>%

  # remove unneeded columns and rename in one pass
  rename(
    lab_sample      = lab_sample_id,
    detection_limit = dl,
    sample          = sample_id
  ) %>%
  # Parse combined datetime strings and extract date/time components.
  # NOTE: SGS data has date and time combined; ALS has date only.
  # dplyr mutate() processes columns in order, so intermediate columns
  # (e.g., collect_date_time) can be referenced later in the same call.
  # This replaces the original two-step transform()+mutate() pattern.
  mutate(
    lab_sample          = as.character(lab_sample),
    sample_rpd          = as.character(sample_rpd),
    lab_name            = "SGS North America, Anchorage, Alaska",
    matrix              = "Water",
    collect_date_time   = mdy_hm(collect_date),
    collect_time        = as_hms(collect_date_time),
    collect_date        = date(collect_date_time),
    rec_date_time       = mdy_hm(rec_date),
    rec_time            = as_hms(rec_date_time),
    rec_date            = date(rec_date_time),
    run_date_time       = mdy_hm(run_date_time),
    run_time            = as_hms(run_date_time),
    run_date            = date(run_date_time),
    extracted_date_time = mdy_hm(extracted_date),
    extracted_time      = as_hms(extracted_date_time),
    extracted_date      = date(extracted_date_time)
  ) %>%
  select(-collect_date_time, -rec_date_time, -run_date_time, -extracted_date_time)

# Correct SGS EDD unit error for dissolved Calcium, Iron, and Magnesium
#
# The summer 2021 SGS EDD reports dissolved Ca, Fe, and Mg (method EP200.8)
# with a unit label of "ug/L", but the numeric values are on the mg/L scale.
# For example, Calcium at KR RM 0 NNC is recorded as 16,200 "ug/L" - which
# is 16.2 mg/L, a physically plausible concentration for the Kenai River.
# 16.2 ug/L would be unrealistically low for freshwater calcium.
#
# Cross-referencing with the ALS total metals data (which correctly labels mg/L)
# and the SGS PDF lab reports confirms that the EDD unit label is wrong:
# the values themselves are correct but the unit should be mg/L.
#
# Spring 2021 SGS data includes no dissolved Ca/Fe/Mg runs (confirmed), so this
# correction applies to summer 2021 observations only. Check for future years.
#
# This correction is applied here at ingestion so all downstream calculations -
# including hardness and hardness-dependent regulatory thresholds for Cd, Cr,
# Cu, Pb, and Zn - use the correct unit and value.
sgs_raw %<>%
  mutate(
    result = case_when(
      analyte %in% c("Calcium", "Iron", "Magnesium") & units == "ug/L" ~ result / 1000,
      TRUE ~ result
    ),
    units = case_when(
      analyte %in% c("Calcium", "Iron", "Magnesium") & units == "ug/L" ~ "mg/L",
      TRUE ~ units
    )
  )

rm(spring_sgs_raw, summer_sgs_raw)


###################### Part B: ALS Data Read In #############################

## SGS subcontracted analyses of Ca, Fe, and Mg to ALS laboratories (Kelso, WA).
## These results are not included in the spreadsheet download from SGS engage
## and were entered manually into separate spring and summer "ALS" named spreadsheets.

spring_als_raw <- read.csv(file.path(spring_data_dir, "SGS", paste0("spring_", year, "_als_batch_info.csv"))) %>%
  clean_names()
summer_als_raw <- read.csv(file.path(summer_data_dir, "SGS", paste0("summer_", year, "_als_batch_info.csv"))) %>%
  clean_names()

# bind spring and summer
als_raw <- bind_rows(spring_als_raw, summer_als_raw) %>%
  remove_empty() %>%

  # proceed left to right of existing ALS dataframe to make its naming structure
  # match the sgs_raw dataframe. Add, remove, modify column names as needed.
  select(-client,
         -project,
         -service_request) %>%
  rename(lab_sample = lab_code) %>%
  rename(
    collect_date = date_collected,
    collect_time = time_collected,
    rec_date = date_received,
    rec_time = time_received,
    extracted_date = date_extracted,
    extracted_time = time_extracted,
    extraction_code = extraction_method,
    run_date = date_analyzed,
    run_time = time_analyzed,
    analytical_method = method,
    analyte = component,
    resultflag = result_notes,
    amount_spiked = spike_concentration,
    percent_recovered = percent_recovery,
    allowable_limit = acceptance_limits,
    sample_rpd = rpd,
    loq = reporting_limit) %>%

  mutate(lab_name = "ALS Environmental, Kelso, Washington") %>%
  # prep column classes to bind with sgs dataframe
  transform(analytical_method = as.character(analytical_method),
            run_date = mdy(run_date),
            run_time = as_hms(as.POSIXct(run_time, format = "%H:%M")),
            collect_date = mdy(collect_date),
            rec_date = mdy(rec_date),
            rec_time = as_hms(as.POSIXct(rec_time, format = "%H:%M")),
            extracted_date = mdy(extracted_date),
            extracted_time = as_hms(as.POSIXct(extracted_time, format = "%H:%M")),
            result = as.double(result),
            collect_time = as_hms(as.POSIXct(collect_time, format = "%H:%M")))


# join SGS data with ALS data
dat <- bind_rows(sgs_raw, als_raw)

# remove old dataframes
rm(als_raw, sgs_raw, spring_als_raw, summer_als_raw)


# export table of sample types and manually translate abbreviations
sample_types <- dat %>%
  select(sample_type, lab_name) %>%
  distinct()

# remove old version and write new one
unlink("other/input/wqx_templates/sample_type_abbreviations.xlsx")
write.xlsx(sample_types, "other/input/wqx_templates/sample_type_abbreviations.xlsx")
# manually created a translation of all the acronyms in an accompanying file;
# removed inconsistencies in sample type abbreviations into one consistent schema
# between SGS and ALS labs.


############### Part C: Address spelling/format issues and inconsistent sample/site names ######################

# Upon visual inspection of site names, the location names in the WQX template differ
# slightly from the place names in the SGS report (spelling and name inconsistencies).

# 3/28/2022 - A note on "Duplicate" designations.
# We must distinguish "Field Duplicates" (two field collections at same location/day/time)
# from "Lab Duplicates" (ALS designation "DUP1" in sample_type). Field duplicates are
# designated as "Field Duplicate" in sample_condition.

# move info about duplicate sample and/or sample blank status into separate new column
dat %<>%
  mutate(sample_condition = case_when(
    grepl("Method Blank", sample) ~ "Method Blank",
    grepl("Trip Blank", sample) ~ "Trip Blank",
    grepl("DUP", sample) ~ "Field Duplicate",
    grepl("Dup", sample) ~ "Field Duplicate"))

# remove from "sample" names the text containing the suffixes Diss/Dis (Dissolved metals)
# since we only want location info in this column.
dat %<>%
  mutate(sample = (str_replace(sample, "Diss|Dis|DUP|Dup", ""))) %>%

  # remove "Diss" suffix and "EP" prefix from "analytical_method" column
  mutate(analytical_method = str_replace(analytical_method, "Diss", "")) %>%
  # note trailing space after "EP200.8 "
  mutate(analytical_method = str_replace(analytical_method, "EP200.8 ", "200.8")) %>%

  # address the one stubborn site name still containing "Diss"
  mutate(sample = case_when(
    sample == "RM0-No Name Creek  Diss" ~ "RM0-No Name Creek",
    TRUE ~ sample))


# Sample name clean up: remove white spaces, apostrophes, and dashes
dat %<>%
  mutate(sample = str_trim(sample, "both")) %>%
  mutate(sample = str_squish(sample)) %>%

  # make remaining white spaces underscores
  mutate(sample = gsub("\\s+", "_", sample)) %>%

  # remove apostrophes
  mutate(sample = gsub("\\'", "", sample)) %>%

  # replace dashes with underscores
  mutate(sample = gsub("\\-", "_", sample)) %>%

  # replace multiple underscores with single
  mutate(sample = gsub("\\__", "_", sample)) %>%
  mutate(sample = gsub("\\___", "_", sample)) %>%
  mutate(sample = gsub("\\__", "_", sample))

# Apply note regarding trip blanks (for BTEX organics).
# Crew assignments are year-specific and stored in a per-year CSV:
#   other/input/wqx_templates/trip_blank_crews_{year}.csv
# To add a new year: create that CSV with columns blank_id and note.
# The number of rows can vary (2, 3, or 4 blanks). Non-blank rows get NA.
trip_blank_crews <- read.csv(
  file.path("other/input/wqx_templates", paste0("trip_blank_crews_", year, ".csv"))
)

dat <- dat |>
  mutate(blank_id = str_extract(sample, "Trip_Blank_\\d+")) |>
  left_join(trip_blank_crews, by = "blank_id") |>
  select(-blank_id)


############## Part D: Prepare SGS/ALS Location/Site Names ##########################

# NOTE: SGS/ALS sample name results have misspellings and typos. Provide labs a CSV
# of site names each year so they can use canonical names in future deliveries.

# generate list of unique site names from SGS data for this year
sgs_sitenames <- data.table(unique(dat$sample)) %>%
  arrange(V1)

# generate list of unique site names from WQX template (desired final names)
wqx_sitenames <- read_excel("other/input/wqx_templates/AWQMS_KWF_Baseline_2021.xlsx",
                             sheet = "Monitoring Locations") %>%
  select("Monitoring Location Name", "Monitoring Location ID") %>%
  distinct()

# write SGS site names to an excel file
site_match_table_path <- "other/input/wqx_templates/sgs_site_names_matching_table.xlsx"
write.xlsx(sgs_sitenames, site_match_table_path)

# create an excel file with two sheets: a.) SGS site names, and b.) WQX site names
wb <- loadWorkbook(site_match_table_path)
addWorksheet(wb, "Sheet2")
writeData(wb, "Sheet2", wqx_sitenames)
saveWorkbook(wb, site_match_table_path, overwrite = TRUE)

# Using these two tables, manually create "sgs_site_names_matching_table_manual_edit.xlsx"
# and match up the two disparate naming systems.
# Site name matching performed manually by B Meyer, March 18, 2022.

## read in site names join table
sitenames_match <- read_excel("other/input/wqx_templates/sgs_site_names_matching_table_manual_edit.xlsx") %>%
  select(`Monitoring Location Name`, `Monitoring Location ID`, sgs_sitenames) %>%
  rename(sample = sgs_sitenames) %>%
  filter(!is.na(`Monitoring Location ID`))

# append monitoring location names
dat %<>%
  left_join(sitenames_match, by = "sample") %>%
  clean_names()

# remove extraneous dataframes
rm(sgs_sitenames, wqx_sitenames, sitenames_match)


######################## Part E: "Result Analytical Method Context" name rectification ######################

# The EPA names for chemical analyses in "Result Analytical Method ID" do not exactly
# match the names provided by SGS. After communicating with SGS and ADEC on 2/8/2022,
# we are able to cross-walk between the two naming systems. Matches are documented in
# "analysis_code_matching_table.xlsx."

# read in matching table
analysis_code_matching_table <- read_excel("other/input/wqx_templates/analysis_code_matching_table.xlsx") %>%
  select(-Comments, -`EPA Name`) %>%
  clean_names() %>%
  rename(analytical_method = sgs_analysis_code) %>%
  # remove "EP" prefix from method "EP200.8"
  mutate(analytical_method = str_replace(analytical_method, "EP200.8", "200.8"))

# read in WQX analytical methods list
wqx_analytical_methods <- read_excel("other/input/wqx_templates/AWQMS_KWF_Baseline_2021.xlsx",
                                      sheet = "Analytical Methods") %>%
  select("ID", "Context Code") %>%
  clean_names() %>%
  rename(epa_analysis_id = id) %>%
  distinct()

# join two tables above
epa_analysis_codes <- inner_join(wqx_analytical_methods, analysis_code_matching_table,
                                  by = "epa_analysis_id") %>%
  filter(!context_code %in% c("USEPA Rev 5.4", "APHA (1997)", "APHA (1999)"))

# join EPA analysis IDs and context codes to overall dataset
dat %<>%
  mutate(analytical_method = str_replace(analytical_method, "EP200.8", "200.8")) %>%
  left_join(epa_analysis_codes, by = "analytical_method")

# remove unneeded dfs
rm(analysis_code_matching_table, wqx_analytical_methods, epa_analysis_codes)


########################## Miscellaneous Steps for SGS/ALS values #########################################

########### Address Non-Detect values #########################

# Non-detect values should be left blank. A non-detect does not necessarily mean
# there was a zero observation - it could be present below the method detection level.
# Instead of putting 0 in the results, we'll leave it blank. The LOQ is presented
# alongside the result. When DEC evaluates a waterbody, they'll use 1/2 the PQL as a
# stand-in for a non-detect.
# See "other/documents/references/SGS DL, LOD, LOQ Interpretation.pdf" for details.

# modify non-detect values from "0" to "NA" if resultflag = U or ND
dat %<>%
  mutate(result1 = na_if(result, 0)) %>%
  select(-result) %>%
  rename(result = result1)


###### Segregate laboratory QA/QC data from field data ########

# These lab-only data will be evaluated at a later step of QA/QC evaluation.
# See "other/input/wqx_templates/sample_type_abbreviations_manual_edit.xlsx"
# for the sample_type naming schema.

sgs_als_qaqc_dat <- dat %>%
  # retain only results NOT from field sampling program (project samples and trip blanks)
  # also filter out hydrocarbon surrogate results ("surr") - these are compounds spiked
  # into samples to monitor extraction/calibration systems; not environmental observations.
  filter(!sample_type %in% c("PS", "SMPL", "TB") |
           grepl("(surr)", analyte))

write.csv(sgs_als_qaqc_dat, file.path(output_qaqc_dir, "sgs_als_qaqc_dat.csv"))

# retain only field sample results for WQX export
dat %<>%
  filter(sample_type %in% c("PS", "SMPL", "TB")) %>%
  filter(!grepl("(surr)", analyte))

rm(sgs_als_qaqc_dat)
