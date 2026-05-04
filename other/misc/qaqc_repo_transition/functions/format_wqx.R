# format_wqx.R
#
# Applies all lookup table joins (coordinates, result sample fraction, detection
# condition, preservative, container), constructs WQX-formatted column names,
# builds Activity IDs, saves dat_raw for downstream QA/QC use, and writes the
# provisional WQX intermediate CSV.
#
# Requires in global env:
#   dat         -- standardized field results (see dat column contract in AGENTS.md)
#   cfg         -- list with keys: templates_dir, wqx_template_file, wqx_intermediate_path
#   year        -- integer data year (also used for Activity ID)
#   spring_sample_date -- character "M/D/YYYY"
#
# Produces in global env:
#   dat      (WQX-formatted columns, reduced to template column set; raw cols restored)
#   dat_raw  (pre-WQX snapshot with original column names for downstream QA/QC)
#   all_dat  (full-width WQX-formatted dat before column reduction)
#
# Also writes:
#   cfg$wqx_intermediate_path  (provisional WQX CSV, not yet flagged)


############### Miscellaneous Steps for Overall Field Results Dataframe #################

############ a.) filter out lab blanks and positive lab controls ##################
# (Already filtered upstream in Part A -- this is a safety note only)
# dat %<>%
#   filter(!sample_condition %in% c("Lab Blank","Positive Control"))


############ b.) match latitude and longitude coordinates to sites ##################
site_coords <- read_excel(
  file.path(cfg$templates_dir, "wqx_template_matching_table.xlsx"),
  sheet = "site_coordinates"
) %>%
  remove_empty() %>%
  mutate(monitoring_location_id = as.character(monitoring_location_id))
dat <- left_join(dat, site_coords)


############ c.) assign "result sample fraction" (e.g. filtered, dissolved, etc.) ############
# If Part A already set result_sample_fraction (e.g., to distinguish dissolved from total
# for EP200.8), preserve those values and use the lookup table only as a fallback.
rsf_part_a <- dat[["result_sample_fraction"]]

result_sample_fraction_lkp <- read_excel(
  file.path(cfg$templates_dir, "wqx_template_matching_table.xlsx"),
  sheet = "result_sample_fraction"
) %>%
  filter(!is.na(analytical_method)) %>%
  select(-description) %>%
  rename(rsf_lookup = result_sample_fraction)

# Drop any existing result_sample_fraction before join to avoid column conflict
dat[["result_sample_fraction"]] <- NULL
dat <- left_join(dat, result_sample_fraction_lkp)

# Restore Part A values where present; fall back to lookup table otherwise
dat <- dat %>%
  mutate(result_sample_fraction = dplyr::coalesce(rsf_part_a, rsf_lookup)) %>%
  select(-rsf_lookup)

# TSS and FC overrides (applied after lookup and Part A values are reconciled):
# - TSS (SM 2540-D): "Suspended"
# - Fecal Coliform (9222D): "None"
dat <- dat %>%
  mutate(result_sample_fraction = case_when(
    grepl("2540", analytical_method, ignore.case = TRUE) ~ "Suspended",
    grepl("9222", analytical_method, ignore.case = TRUE) ~ "None",
    TRUE ~ result_sample_fraction
  ))

rm(rsf_part_a, result_sample_fraction_lkp)


############ d.) assign "result detection condition" ##############
# Left join approach was unreliable; define programmatically instead.
dat %<>%
  mutate(result_detection_condition = case_when(
    resultflag == "U" | resultflag == "ND" ~ "Not Detected",
    resultflag == "J"                       ~ "Present Below Quantification Limit"
  ))


############ e.) assign chemical preservative type ################
chemical_preservative <- read_excel(
  file.path(cfg$templates_dir, "wqx_template_matching_table.xlsx"),
  sheet = "chemical_preservative"
) %>%
  filter(!is.na(preservative)) %>%
  select(-description)
dat <- left_join(dat, chemical_preservative)


############ f.) assign bottle type and color ######################
bottle_type_color <- read_excel(
  file.path(cfg$templates_dir, "wqx_template_matching_table.xlsx"),
  sheet = "sample_container_type_color"
) %>%
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
write.csv(
  data.frame(unique(dat$analyte)),
  file.path(cfg$templates_dir, "analytes_list.csv"),
  row.names = FALSE
)
# Abbreviations are manually assigned in "analytes_list_manual_edit.csv"
analyte_abbrev <- read.csv(file.path(cfg$templates_dir, "analytes_list_manual_edit.csv"))
colnames(analyte_abbrev) <- c("analyte", "analyte_abbreviation")
analyte_abbrev %<>% select(analyte, analyte_abbreviation)
dat %<>% left_join(analyte_abbrev)

## b.) sample_condition abbreviations
dat %<>%
  mutate(sample_condition_abbrv = case_when(
    sample_condition == "Field Duplicate" ~ "DUP",
    sample_condition == "Trip Blank"      ~ "Blank"
  ))


############ i.) prepare final WQX column format ###############################
# Create column structure from the WQX template. Column names must match exactly.
# Save raw-column-name dat before WQX renaming so downstream QA/QC chunks
# (which expect original column names like analyte, collect_date, etc.) work correctly.
dat_raw <- dat

dat %<>%
  mutate(
    `Monitoring Location ID`              = monitoring_location_id,
    `Activity Media Name`                 = "Water",
    `Activity Media Subdivision Name`     = "Surface Water",
    # create Activity ID conditionally on sample_condition presence
    `Activity ID` = case_when(
      is.na(sample_condition) ~ paste0(`Monitoring Location ID`, "-",
                                        collect_date, "-", analyte_abbreviation),
      !is.na(sample_condition) ~ paste0(`Monitoring Location ID`, "-",
                                         collect_date, "-", analyte_abbreviation,
                                         "-", sample_condition_abbrv)
    ),
    `Activity Start Date`                 = collect_date,
    `Activity Start Time`                 = collect_time,
    `Activity End Date`                   = "",
    `Activity End Time`                   = "",
    `Activity Latitude`                   = latitude,
    `Activity Longitude`                  = longitude,
    `Activity Source Map Scale`           = "",
    `Activity Type` = case_when(
      sample_condition == "Field Duplicate" ~ "Quality Control Field Replicate Msr/Obs",
      sample_condition == "Blank"           ~ "Quality Control Sample-Trip Blank",
      sample_type == "TB"                   ~ "Quality Control Sample-Trip Blank",
      TRUE ~ "Field Msr/Obs"
    ),
    # All samples are surface grab samples; depth assigned as ~6 inches (~15 cm)
    `Activity Depth/Height Measure`       = 15,
    `Activity Depth/Height Unit`          = "cm",
    `Activity Top Depth/Height Measure`   = "",
    `Activity Top Depth/Height Unit`      = "",
    `Activity Bottom Depth/Height Measure` = "",
    `Activity Bottom Depth/Height Unit`   = "",
    `Activity Relative Depth Name`        = "",
    `Activity Comment`                    = note,
    `Characteristic Name`                 = analyte,
    `Result Analytical Method ID`         = epa_analysis_id,
    `Result Analytical Method Context`    = context_code,
    `Method Speciation`                   = "",
    `Result Value`                        = result,
    `Result Unit`                         = units,
    `Result Qualifier`                    = resultflag,
    `Result Weight Basis`                 = "Sampled",
    `Statistical Base Code`               = stat_base_code,
    `Result Sample Fraction`              = result_sample_fraction,
    `Result Value Type`                   = "Actual",
    `Result Comment`                      = "",
    `Sample Collection Method ID`         = "",
    `Equipment ID`                        = "Water Bottle",
    `Result Detection Condition`          = result_detection_condition,
    `Result Detection Limit Type 1`       = "Limit of Quantitation",
    `Result Detection Limit Value 1`      = loq,
    `Result Detection Limit Unit 1`       = units,
    # lod = "limit of detection" = "method detection level"
    `Result Detection Limit Type 2`       = "Method Detection Level",
    `Result Detection Limit Value 2`      = lod,
    `Result Detection Limit Unit 2`       = units,
    `Laboratory Accreditation Indicator`  = "",
    `Laboratory Name`                     = lab_name,
    `Laboratory Sample ID`                = lab_sample,
    `Analysis Start Date`                 = run_date,
    `Analysis Start Time`                 = run_time,
    `Biological Intent`                   = "",
    `Subject Taxonomic Name`              = "",
    `Thermal Preservative`                = "Cold packs",
    `Sample Container Type`               = sample_container_type,
    `Sample Container Color`              = sample_container_color,
    `Chemical Preservative`               = preservative,
    .keep = "unused"
  )

# Save a full-width copy before reducing to template columns (used in QA/QC analyses)
all_dat <- dat

# Reduce to just those columns listed in the WQX template
wqx_colnames <- read_excel(cfg$wqx_template_file,
                             sheet = "KWF Baseline AWQMS Template") %>%
  colnames()
dat %<>% select(one_of(wqx_colnames))

# Export provisional WQX-formatted results
write.csv(dat, cfg$wqx_intermediate_path, row.names = FALSE)

# Restore dat with raw column names so downstream QA/QC chunks work correctly.
dat <- dat_raw
rm(dat_raw)
