# format_wqx.R
#
# Applies all lookup table joins (coordinates, result sample fraction, detection
# condition, preservative, container), constructs WQX-formatted column names,
# builds Activity IDs, saves dat_raw for downstream QA/QC use, and writes the
# provisional WQX intermediate CSV.
#
# Requires in global env (set in appendix_a.qmd year-config block):
#   dat, year, spring_sample_date, wqx_intermediate_path
#
# Produces in global env:
#   dat      (WQX-formatted columns, reduced to template column set)
#   dat_raw  (pre-WQX snapshot with original column names for downstream QA/QC)
#   all_dat  (full-width WQX-formatted dat before column reduction)
#
# Also writes:
#   wqx_intermediate_path  (provisional WQX CSV, not yet flagged)


############### Miscellaneous Steps for Overall Field Results Dataframe #################

############ a.) filter out lab blanks and positive lab controls ##################
# (Already filtered upstream — commented out as a safety note)
# dat %<>%
#   filter(!sample_condition %in% c("Lab Blank","Positive Control"))


############ b.) match latitude and longitude coordinates to sites ##################
site_coords <- read_excel("other/input/wqx_templates/wqx_template_matching_table.xlsx",
                           sheet = "site_coordinates") %>%
  remove_empty() %>%
  # Guard against stray Unicode dash characters (e.g. U+2010-U+2015, U+2212)
  # sneaking into lat/long text cells from copy-paste; WQX Web requires a
  # plain ASCII hyphen-minus for negative coordinates.
  mutate(longitude = stringr::str_replace_all(longitude, "[\u2010-\u2015\u2212]", "-"))
dat <- left_join(dat, site_coords)


############ c.) assign "result sample fraction" (e.g. filtered, dissolved, etc.) ############
result_sample_fraction <- read_excel("other/input/wqx_templates/wqx_template_matching_table.xlsx",
                                      sheet = "result_sample_fraction") %>%
  filter(!is.na(analytical_method)) %>%
  select(-description)
dat <- left_join(dat, result_sample_fraction)

# The fraction join keys on analytical_method, but TSS and FC use long method strings
# that don't match the lookup table keys. Assign their fractions explicitly.
# - TSS (SM 2540-D): "Suspended" = settled/filtered solids on an unfiltered sample
# - Fecal Coliform (9222D): "None" = no sample fraction applies (whole-water biological)
dat <- dat %>%
  mutate(result_sample_fraction = case_when(
    grepl("2540", analytical_method, ignore.case = TRUE) ~ "Suspended",
    grepl("9222", analytical_method, ignore.case = TRUE) ~ "None",
    TRUE ~ result_sample_fraction
  ))


############ d.) assign "result detection condition" ##############
# Left join approach was unreliable; define programmatically instead.
dat %<>%
  mutate(result_detection_condition = case_when(
    resultflag == "U" | resultflag == "ND" ~ "Not Detected",
    resultflag == "J" ~ "Present Below Quantification Limit"))


############ e.) assign chemical preservative type ################
chemical_preservative <- read_excel("other/input/wqx_templates/wqx_template_matching_table.xlsx",
                                     sheet = "chemical_preservative") %>%
  filter(!is.na(preservative)) %>%
  select(-description)
dat <- left_join(dat, chemical_preservative)


############ f.) assign bottle type and color ######################
bottle_type_color <- read_excel("other/input/wqx_templates/wqx_template_matching_table.xlsx",
                                 sheet = "sample_container_type_color") %>%
  select(-description) %>%
  filter(!is.na(sample_container_type))
dat <- left_join(dat, bottle_type_color)

rm(site_coords, result_sample_fraction, chemical_preservative, bottle_type_color)


############ g.) assign "Statistical Base Code" column ##############
dat %<>%
  mutate(stat_base_code = case_when(
    analyte == "Fecal Coliform" ~ "Count"
  ))


########### h.) "Activity ID" code shortening ######################
# Activity ID = concatenation of Location ID, date, analyte abbreviation,
# and (if present) sample_condition. Max 55 characters.

## a.) analyte abbreviations
write.csv(data.frame(unique(dat$analyte)), "other/input/wqx_templates/analytes_list.csv",
          row.names = FALSE)
# Abbreviations are manually assigned in "analytes_list_manual_edit.csv"
analyte_abbrev <- read.csv("other/input/wqx_templates/analytes_list_manual_edit.csv")
colnames(analyte_abbrev) <- c("analyte", "analyte_abbreviation")
analyte_abbrev %<>% select(analyte, analyte_abbreviation)
dat %<>% left_join(analyte_abbrev)

## b.) sample_condition abbreviations
dat %<>%
  mutate(sample_condition_abbrv = case_when(
    sample_condition == "Field Duplicate" ~ "DUP",
    sample_condition == "Trip Blank" ~ "Blank"))


############ i.) prepare final WQX column format ###############################
# Create column structure from the WQX template. Column names must match exactly.
# Save raw-column-name dat before WQX renaming so downstream QA/QC chunks
# (which expect original column names like analyte, collect_date, etc.) work correctly.
dat_raw <- dat

dat %<>%
  mutate(
    `Monitoring Location ID` = monitoring_location_id,
    `Activity Media Name` = "Water",
    `Activity Media Subdivision Name` = "Surface Water",
    # create activity ID conditionally on sample_condition presence
    `Activity ID` = case_when(
      is.na(sample_condition) ~ paste0(`Monitoring Location ID`, "-", collect_date, "-", analyte_abbreviation),
      !is.na(sample_condition) ~ paste0(`Monitoring Location ID`, "-", collect_date, "-", analyte_abbreviation, "-", sample_condition_abbrv)),
    `Activity Start Date` = collect_date,
    `Activity Start Time` = collect_time,
    `Activity End Date` = "",
    `Activity End Time` = "",
    `Activity Latitude` = latitude,
    `Activity Longitude` = longitude,
    `Activity Source Map Scale` = "",
    `Activity Type` = case_when(
      sample_condition == "Field Duplicate" ~ "Quality Control Field Replicate Msr/Obs",
      sample_condition == "Blank" ~ "Quality Control Sample-Trip Blank",
      sample_type == "TB" ~ "Quality Control Sample-Trip Blank",
      TRUE ~ "Field Msr/Obs"),
    # All samples are surface grab samples; depth assigned as ~6 inches (~15 cm)
    `Activity Depth/Height Measure` = 15,
    `Activity Depth/Height Unit` = "cm",
    `Activity Top Depth/Height Measure` = "",
    `Activity Top Depth/Height Unit` = "",
    `Activity Bottom Depth/Height Measure` = "",
    `Activity Bottom Depth/Height Unit` = "",
    `Activity Relative Depth Name` = "",
    `Activity Comment` = note,
    `Characteristic Name` = analyte,
    `Result Analytical Method ID` = epa_analysis_id,
    `Result Analytical Method Context` = context_code,
    `Method Speciation` = "",
    `Result Value` = result,
    # WQX requires Result Measure Value whenever Result Measure Unit is
    # provided. For non-detects, result is intentionally blank, so Result
    # Unit must be blanked too (unit info for non-detects lives in the
    # Result Detection Limit Unit fields instead).
    `Result Unit` = if_else(is.na(result), NA_character_, units),
    # "ND" is not a valid WQX Result Qualifier; Result Detection Condition
    # ("Not Detected") already conveys this, so drop the redundant qualifier.
    `Result Qualifier` = if_else(resultflag == "ND", NA_character_, resultflag),
    `Result Weight Basis` = "Sampled",
    `Statistical Base Code` = stat_base_code,
    `Result Sample Fraction` = result_sample_fraction,
    `Result Value Type` = "Actual",
    `Result Comment` = "",
    `Sample Collection Method ID` = "",
    `Equipment ID` = "Water Bottle",
    `Result Detection Condition` = result_detection_condition,
    # WQX domain value is "Practical Quantitation Limit"; SGS's "Limit of
    # Quantitation (LOQ)" terminology is explicitly a renaming of the
    # industry-standard PQL (see other/documents/md/DL-LOD-LOQ-Interpretation-SGS.md)
    `Result Detection Limit Type 1` = "Practical Quantitation Limit",
    `Result Detection Limit Value 1` = loq,
    `Result Detection Limit Unit 1` = units,
    # lod = "limit of detection" = "method detection level"
    # WQX requires a Detection Limit Value whenever a Detection Limit Type is
    # provided; lod is not reported for every analyte/method, so only
    # populate Type 2/Unit 2 when a value is actually available.
    `Result Detection Limit Type 2` = if_else(is.na(lod), NA_character_, "Method Detection Level"),
    `Result Detection Limit Value 2` = lod,
    `Result Detection Limit Unit 2` = if_else(is.na(lod), NA_character_, units),
    `Laboratory Accreditation Indicator` = "",
    `Laboratory Name` = lab_name,
    `Laboratory Sample ID` = lab_sample,
    `Analysis Start Date` = run_date,
    `Analysis Start Time` = run_time,
    `Biological Intent` = "",
    `Subject Taxonomic Name` = "",
    `Thermal Preservative` = "Cold Packs (4 deg C)",
    `Sample Container Type` = sample_container_type,
    `Sample Container Color` = sample_container_color,
    `Chemical Preservative` = preservative,
    .keep = "unused")

# Save a full-width copy before reducing to template columns (used in QA/QC analyses)
all_dat <- dat

# reduce to just those columns listed in the WQX template
wqx_colnames <- read_excel("other/input/wqx_templates/AWQMS_KWF_Baseline_2021.xlsx",
                            sheet = "KWF Baseline AWQMS Template") %>%
  colnames()
dat %<>% select(one_of(wqx_colnames))

# export provisional WQX-formatted results
write.csv(dat, wqx_intermediate_path, row.names = FALSE)

# Restore dat with raw column names so downstream QA/QC chunks work correctly.
dat <- dat_raw
rm(dat_raw)
