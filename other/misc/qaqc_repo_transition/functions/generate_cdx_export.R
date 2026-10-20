# generate_cdx_export.R
#
# Finalizes results_activities.csv for CDX upload by:
#   - Assigning Result Status ID (Accepted/Rejected) based on flag column
#   - Reformatting columns to match CDX import configuration
#   - Writing results_activities.csv, project.csv, and station.csv
# 
# This script reads export_dat from the global environment. export_dat should
# already have time_zone = "AKDT" applied (done in the Q32 chunk).
#
# REVIEW: This script contains a filter that excludes Phosphorus results from
#
# This script reads export_dat from the global environment. export_dat should
# already have time_zone = "AKDT" applied (done in the Q32 chunk).
#
# REVIEW: This script contains a filter that excludes Phosphorus results from
# EPA Method 200.8. This filter was valid for 2021 because Method 200.8 was not
# in the QAPP for phosphorus that year. Confirm whether this filter applies to
# the current year before running. If not needed, comment it out.
#
# Requires in global env:
#   export_dat  (from apply_qaqc_flags.R, refreshed in Q32 timezone chunk)
#   cfg         -- list with keys:
#     cfg$templates_dir      (for wqx_template_matching_table.xlsx and analytes_list_manual_edit.csv)
#     cfg$wqx_template_file  (for WQX column order)
#     cfg$wqx_downloads_dir  (for project.csv and station.csv downloads)
#
# Produces in global env: (none -- all output is written to disk)
#
# Writes:
#   other/output/results_activities.csv
#   other/output/project.csv
#   other/output/station.csv
#
# ID RULE (learned 2026-10-14/20): never put the "KENAI_WQX-" org prefix in any
# ID column (Monitoring Location ID, MonitoringLocationIdentifier, Activity ID).
# WQX Web prepends the prefix itself on import. A prefixed ID is stored as
# "KENAI_WQX-KENAI_WQX-..." and creates duplicate stations that hold the results.
# Checks near each write below stop the script before a bad file is written.
# Details: other/output/epa_wqp_uploads/2026-10-20_doubled_station_cleanup/README.md
# in the kenai-river-wqx report repo.

# Stops with an error if any value starts with the org prefix
assert_no_org_prefix <- function(x, label) {
  bad <- !is.na(x) & grepl("^KENAI_WQX", x, ignore.case = TRUE)
  if (any(bad)) {
    stop(sum(bad), " value(s) in ", label, " start with the org prefix 'KENAI_WQX'. ",
         "WQX Web adds the prefix on import; strip it before upload. First offender: ",
         x[bad][1], call. = FALSE)
  }
  invisible(TRUE)
}


# ── Results and Activities ──────────────────────────────────────────────────────

# Label flagged observations as "Rejected," non-flagged as "Accepted"
export_dat %<>%
  mutate(`Result Status ID` = case_when(
    flag == "Y" ~ "Rejected",
    flag == "N" ~ "Accepted"
  ))

# REVIEW: Exclude Phosphorus results from EPA Method 200.8 (Dissolved Metals by ICP/MS).
# This method was not part of the 2021 QAPP; only Total Phosphorus via SM21 4500-P-E
# was planned and approved. These results are retained in the flagged intermediate
# file but are not submitted to CDX.
# Confirm this applies for the current year before keeping this filter.
export_dat %<>%
  filter(!(characteristic_name == "Phosphorus" & result_analytical_method_id == "200.8"))

# Re-join site codes needed for Activity ID construction.
# NOTE: in this sheet the column "MonitoringLocationIdentifier" holds the site
# CODE (e.g. KBL_m_01.5), not a WQP identifier, and "MonitoringLocationName" is a
# descriptive name. Rename to avoid confusion and join explicitly on the numeric ID.
site_name_match <- read_excel(
  file.path(cfg$templates_dir, "wqx_template_matching_table.xlsx"),
  sheet = "adec_site_names"
) %>%
  remove_empty() %>%
  transmute(monitoring_location_id = as.character(monitoring_location_id),
            site_code = MonitoringLocationIdentifier)

# The 22 valid bare location IDs, used by the checks below
valid_location_ids <- site_name_match$monitoring_location_id

export_dat %<>% mutate(monitoring_location_id = as.character(monitoring_location_id))
export_dat <- left_join(export_dat, site_name_match, by = "monitoring_location_id")

if (any(is.na(export_dat$site_code))) {
  stop("Some rows have no site code in the 'adec_site_names' lookup: ",
       paste(unique(export_dat$monitoring_location_id[is.na(export_dat$site_code)]),
             collapse = ", "), call. = FALSE)
}

# Re-join analyte abbreviations for Activity ID construction
analyte_abbrev <- read.csv(file.path(cfg$templates_dir, "analytes_list_manual_edit.csv"))
colnames(analyte_abbrev) <- c("characteristic_name", "analyte_abbreviation")
analyte_abbrev %<>% select(characteristic_name, analyte_abbreviation)
export_dat %<>% left_join(analyte_abbrev)

# Extract sample_condition abbreviation from existing activity_id column
export_dat %<>%
  mutate(sample_condition_abbrv = case_when(
    grepl("DUP",   activity_id) ~ "DUP",
    grepl("Blank", activity_id) ~ "Blank"
  ))

# Finalize column structure to match CDX import configuration
export_dat %<>%
  mutate(
    `Project ID`                           = 10000007,
    # Bare numeric ID (no org prefix); WQX Web prepends "KENAI_WQX-" on import
    `Monitoring Location ID`               = monitoring_location_id,
    `Activity Media Name`                  = activity_media_name,
    `Activity Media Subdivision Name`      = activity_media_subdivision_name,
    `Activity ID` = case_when(
      is.na(sample_condition_abbrv) ~
        paste0(site_code, "-", activity_start_date, "-", analyte_abbreviation),
      !is.na(sample_condition_abbrv) ~
        paste0(site_code, "-", activity_start_date, "-",
               analyte_abbreviation, "-", sample_condition_abbrv)
    ),
    `Activity Start Date`                  = activity_start_date,
    `Activity Start Time`                  = activity_start_time,
    `Activity Start Time Zone`             = "AKDT",
    `Activity End Date`                    = activity_end_date,
    `Activity End Time`                    = activity_end_time,
    `Activity End Time Zone`               = "AKDT",
    `Activity Latitude`                    = activity_latitude,
    `Activity Longitude`                   = activity_longitude,
    `Activity Source Map Scale`            = activity_source_map_scale,
    `Activity Type`                        = activity_type,
    `Activity Depth/Height Measure`        = activity_depth_height_measure,
    `Activity Depth/Height Unit`           = activity_depth_height_unit,
    `Activity Top Depth/Height Measure`    = activity_top_depth_height_measure,
    `Activity Top Depth/Height Unit`       = activity_top_depth_height_unit,
    `Activity Bottom Depth/Height Measure` = activity_bottom_depth_height_measure,
    `Activity Bottom Depth/Height Unit`    = activity_bottom_depth_height_unit,
    `Activity Relative Depth Name`         = activity_relative_depth_name,
    `Activity Comment`                     = activity_comment,
    `Characteristic Name`                  = characteristic_name,
    `Result Analytical Method ID`          = result_analytical_method_id,
    `Result Analytical Method Context`     = result_analytical_method_context,
    `Method Speciation`                    = method_speciation,
    `Result Value`                         = result_value,
    `Result Unit`                          = result_unit,
    `Result Qualifier`                     = result_qualifier,
    `Result Weight Basis`                  = result_weight_basis,
    `Statistical Base Code`                = statistical_base_code,
    `Result Sample Fraction`               = result_sample_fraction,
    `Result Value Type`                    = result_value_type,
    `Result Comment`                       = result_comment,
    `Sample Collection Method ID`          = sample_collection_method_id,
    `Equipment ID`                         = equipment_id,
    `Result Detection Condition`           = result_detection_condition,
    `Result Detection Limit Type 1`        = result_detection_limit_type_1,
    `Result Detection Limit Value 1`       = result_detection_limit_value_1,
    `Result Detection Limit Unit 1`        = result_detection_limit_unit_1,
    `Result Detection Limit Type 2`        = result_detection_limit_type_2,
    `Result Detection Limit Value 2`       = result_detection_limit_value_2,
    `Result Detection Limit Unit 2`        = result_detection_limit_unit_2,
    `Laboratory Accreditation Indicator`   = laboratory_accreditation_indicator,
    `Laboratory Name`                      = laboratory_name,
    `Laboratory Sample ID`                 = laboratory_sample_id,
    `Analysis Start Date`                  = analysis_start_date,
    `Analysis Start Time`                  = analysis_start_time,
    `Biological Intent`                    = biological_intent,
    `Subject Taxonomic Name`               = subject_taxonomic_name,
    `Thermal Preservative`                 = thermal_preservative,
    `Sample Container Type`                = sample_container_type,
    `Sample Container Color`               = sample_container_color,
    `Chemical Preservative`                = chemical_preservative,
    .keep = "unused"
  )

# Reduce to template column set
wqx_colnames <- read_excel(cfg$wqx_template_file,
                             sheet = "KWF Baseline AWQMS Template") %>%
  colnames()
export_dat %<>% select(one_of(wqx_colnames))

# Checks before writing: ID form
assert_no_org_prefix(export_dat$`Monitoring Location ID`, "Monitoring Location ID")
assert_no_org_prefix(export_dat$`Activity ID`, "Activity ID")
stopifnot(
  "Monitoring Location ID must be one of the 22 known bare IDs" =
    all(as.character(export_dat$`Monitoring Location ID`) %in% valid_location_ids),
  "Activity ID must not be NA" = !anyNA(export_dat$`Activity ID`),
  "Activity ID must be 55 characters or fewer" = all(nchar(export_dat$`Activity ID`) <= 55)
)

# Write final results and activities CSV for CDX upload
write.csv(export_dat, here::here("other/output/results_activities.csv"), row.names = FALSE)


# ── Project CSV ──────────────────────────────────────────────────────────────────
# Read from the WQP download, then apply corrections needed for CDX upload:
#   - Keep only the Agency Baseline project row
#   - Update QAPP approval status and approving agency (v3 approved by EPA Region 10, May 2023)
#   - Set ProjectDescriptionText to the project home page URL
#   - Attach the QAPP PDF filename and type
#
# REVIEW: Update ProjectAttachmentFileName if a newer QAPP version was approved.
project_upload <- read.csv(
  file.path(cfg$wqx_downloads_dir, "project/project.csv")
) |>
  dplyr::filter(ProjectName == "Agency Baseline") |>
  dplyr::mutate(
    QAPPApprovedIndicator     = "Yes",
    QAPPApprovalAgencyName    = "EPA",
    ProjectDescriptionText    = "https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/",
    ProjectAttachmentFileName = "KenaiWatershedForum_QAPP_v3_2023_with_Addendum_April_2024.pdf",
    ProjectAttachmentType     = "QAPP"
  ) |>
  dplyr::select(
    ProjectIdentifier,
    ProjectName,
    ProjectDescriptionText,
    QAPPApprovedIndicator,
    QAPPApprovalAgencyName,
    ProjectAttachmentFileName,
    ProjectAttachmentType
  )

write.csv(project_upload, here::here("other/output/project.csv"), row.names = FALSE)


# ── Station CSV ──────────────────────────────────────────────────────────────────
# Read from the WQP download, filter to KWF Kenai Baseline sites (prefix "KBL"),
# and retain only the columns required by the CDX import configuration.
station_upload <- read.csv(
  file.path(cfg$wqx_downloads_dir, "station/station.csv")
) |>
  dplyr::filter(grepl("KBL", MonitoringLocationName)) |>
  # WQP IDs carry the "KENAI_WQX-" prefix; WQX Web adds it back on import
  dplyr::mutate(MonitoringLocationIdentifier = sub("^KENAI_WQX-", "", MonitoringLocationIdentifier)) |>
  dplyr::select(
    MonitoringLocationIdentifier,
    MonitoringLocationName,
    MonitoringLocationTypeName,
    MonitoringLocationDescriptionText,
    HUCEightDigitCode,
    LatitudeMeasure,
    LongitudeMeasure,
    HorizontalCollectionMethodName,
    HorizontalCoordinateReferenceSystemDatumName,
    StateCode,
    CountyCode
  ) |>
  # "Interpolation-Satellite" and "Unknown" are not valid WQX domain values;
  # "GPS-Unspecified" is correct for satellite-based coordinates (what ADEC uses).
  # Both are corrected here as a safety net for future WQP re-downloads.
  dplyr::mutate(HorizontalCollectionMethodName = dplyr::case_when(
    HorizontalCollectionMethodName %in% c("Interpolation-Satellite", "Unknown") ~ "GPS-Unspecified",
    TRUE ~ HorizontalCollectionMethodName
  ))

# Checks before writing: ID form. A leftover prefix here usually means the WQP
# download still contains the doubled "KENAI_WQX-KENAI_WQX-..." stations.
assert_no_org_prefix(station_upload$MonitoringLocationIdentifier, "station MonitoringLocationIdentifier")
stopifnot(
  "station.csv must contain exactly the 22 known station IDs, once each" =
    setequal(as.character(station_upload$MonitoringLocationIdentifier), valid_location_ids) &&
    !anyDuplicated(station_upload$MonitoringLocationIdentifier)
)

write.csv(station_upload, here::here("other/output/station.csv"), row.names = FALSE)
