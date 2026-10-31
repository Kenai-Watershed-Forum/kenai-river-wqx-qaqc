# generate_cdx_export.R
#
# Finalizes results_activities.csv for CDX upload by:
#   - Assigning Result Status ID (Accepted/Rejected) based on flag column
#   - Excluding Phosphorus from Method 200.8 (not in QAPP for 2021)
#   - Reformatting columns to match CDX import configuration
#   - Writing results_activities.csv, project.csv, and station.csv
#
# This script reads export_dat from the global environment. export_dat should
# already have time_zone = "AKDT" applied (done in the Q32 chunk).
#
# Requires in global env:
#   export_dat  (from apply_qaqc_flags.R, refreshed in Q32 timezone chunk)
#
# Produces in global env: (none — all output is written to disk)
#
# Writes:
#   other/output/wqx_formatted/results_activities.csv
#   other/output/wqx_formatted/project.csv
#   other/output/wqx_formatted/station.csv


# ── Results and Activities ──────────────────────────────────────────────────────

# label flagged observations as "Rejected," non-flagged as "Accepted"
export_dat %<>%
  mutate(`Result Status ID` = case_when(
    flag == "Y" ~ "Rejected",
    flag == "N" ~ "Accepted"
  ))

# Exclude Phosphorus results from EPA Method 200.8 (Dissolved Metals by ICP/MS).
# This method was not part of the QAPP; only Total Phosphorus via SM21 4500-P-E
# was planned and approved. These results are retained in the flagged intermediate
# file but are not submitted to CDX.
export_dat %<>%
  filter(!(characteristic_name == "Phosphorus" & result_analytical_method_id == "200.8"))

# re-join WQP site location info (site names) needed for Activity ID construction
site_name_match <- read_excel("other/input/wqx_templates/wqx_template_matching_table.xlsx",
                               sheet = "adec_site_names") %>%
  remove_empty()
export_dat <- left_join(export_dat, site_name_match)

# re-join analyte abbreviations for Activity ID construction
analyte_abbrev <- read.csv("other/input/wqx_templates/analytes_list_manual_edit.csv")
colnames(analyte_abbrev) <- c("characteristic_name", "analyte_abbreviation")
analyte_abbrev %<>% select(characteristic_name, analyte_abbreviation)
export_dat %<>% left_join(analyte_abbrev)

# extract sample_condition abbreviation from existing activity_id column
export_dat %<>%
  mutate(sample_condition_abbrv = case_when(
    grepl("DUP", activity_id) ~ "DUP",
    grepl("Blank", activity_id) ~ "Blank"
  ))

# Translate lab-friendly characteristic names to WQX domain values. Applied
# only here (at final CDX export) so the rest of the pipeline - including the
# Section 3.2 matrix-spike flagging narrative, which references
# "Total Nitrate/Nitrite-N" by name - can keep using the lab's own
# terminology. Confirmed against live WQX Characteristic.CSV domain values:
#   - "Total Nitrate/Nitrite-N" -> "Nitrate + Nitrite" (WQX has no "Total"
#     or "-N" variant; fraction/units already convey this is as N)
#   - "Calcium, Total" / "Magnesium, Total" -> "Calcium" / "Magnesium" (WQX
#     conveys total vs. dissolved via Result Sample Fraction, not the name)
#   - "P & M -Xylene" -> "m,p-Xylene" (WQX id 3585; distinct from "Total
#     xylenes"/id 8052, which is the sum of all three isomers)
export_dat %<>%
  mutate(characteristic_name = case_when(
    characteristic_name == "Total Nitrate/Nitrite-N" ~ "Nitrate + Nitrite",
    characteristic_name == "Calcium, Total" ~ "Calcium",
    characteristic_name == "Magnesium, Total" ~ "Magnesium",
    characteristic_name == "P & M -Xylene" ~ "m,p-Xylene",
    TRUE ~ characteristic_name
  ))

# WQX requires Method Speciation when Characteristic Name resolves (via its
# built-in synonym matching) to "Total Phosphorus, mixed forms"; our
# "Total Phosphorus" input matches that synonym. "as P" is the correct
# WQX MethodSpeciation domain value (confirmed against live domain list).
# Similarly, "Nitrate + Nitrite" requires Method Speciation; the lab's own
# name for this analyte ("Total Nitrate/Nitrite-N") indicates results are
# reported as nitrogen, so "as N" is the correct WQX MethodSpeciation
# domain value (confirmed against live domain list).
export_dat %<>%
  mutate(method_speciation = case_when(
    characteristic_name == "Total Phosphorus" ~ "as P",
    characteristic_name == "Nitrate + Nitrite" ~ "as N",
    TRUE ~ method_speciation
  ))

# finalize column structure to match CDX import configuration
export_dat %<>%
  mutate(
    `Project ID` = 10000007,
    # WQX adds the org prefix itself on import; keeping it creates duplicate
    # "KENAI_WQX-KENAI_WQX-..." stations
    `Monitoring Location ID` = sub("^KENAI_WQX-", "", MonitoringLocationIdentifier),
    `Monitoring Location Name` = MonitoringLocationName,
    `Activity Media Name` = activity_media_name,
    `Activity Media Subdivision Name` = activity_media_subdivision_name,
    `Activity ID` = case_when(
      is.na(sample_condition_abbrv) ~ paste0(`MonitoringLocationName`, "-", activity_start_date, "-", analyte_abbreviation),
      !is.na(sample_condition_abbrv) ~ paste0(`MonitoringLocationName`, "-", activity_start_date, "-", analyte_abbreviation, "-", sample_condition_abbrv)),
    `Activity Start Date` = activity_start_date,
    `Activity Start Time` = activity_start_time,
    `Activity Start Time Zone` = "AKDT",
    `Activity End Date` = activity_end_date,
    `Activity End Time` = activity_end_time,
    `Activity End Time Zone` = "AKDT",
    `Activity Latitude` = activity_latitude,
    `Activity Longitude` = activity_longitude,
    `Activity Source Map Scale` = activity_source_map_scale,
    `Activity Type` = activity_type,
    `Activity Depth/Height Measure` = activity_depth_height_measure,
    `Activity Depth/Height Unit` = activity_depth_height_unit,
    `Activity Top Depth/Height Measure` = activity_top_depth_height_measure,
    `Activity Top Depth/Height Unit` = activity_top_depth_height_unit,
    `Activity Bottom Depth/Height Measure` = activity_bottom_depth_height_measure,
    `Activity Bottom Depth/Height Unit` = activity_bottom_depth_height_unit,
    `Activity Relative Depth Name` = activity_relative_depth_name,
    `Activity Comment` = activity_comment,
    `Characteristic Name` = characteristic_name,
    `Result Analytical Method ID` = result_analytical_method_id,
    `Result Analytical Method Context` = result_analytical_method_context,
    `Method Speciation` = method_speciation,
    `Result Value` = result_value,
    `Result Unit` = result_unit,
    `Result Qualifier` = result_qualifier,
    `Result Weight Basis` = result_weight_basis,
    `Statistical Base Code` = statistical_base_code,
    `Result Sample Fraction` = result_sample_fraction,
    `Result Value Type` = result_value_type,
    `Result Comment` = result_comment,
    `Sample Collection Method ID` = sample_collection_method_id,
    `Equipment ID` = equipment_id,
    `Result Detection Condition` = result_detection_condition,
    `Result Detection Limit Type 1` = result_detection_limit_type_1,
    `Result Detection Limit Value 1` = result_detection_limit_value_1,
    `Result Detection Limit Unit 1` = result_detection_limit_unit_1,
    `Result Detection Limit Type 2` = result_detection_limit_type_2,
    `Result Detection Limit Value 2` = result_detection_limit_value_2,
    `Result Detection Limit Unit 2` = result_detection_limit_unit_2,
    `Laboratory Accreditation Indicator` = laboratory_accreditation_indicator,
    `Laboratory Name` = laboratory_name,
    `Laboratory Sample ID` = laboratory_sample_id,
    `Analysis Start Date` = analysis_start_date,
    `Analysis Start Time` = analysis_start_time,
    # Biological Intent / Subject Taxonomic Name intentionally omitted: this
    # project never collects biological/tissue samples, so these WQX fields
    # are permanently out of scope. Dropping them here (rather than mapping
    # to always-blank columns) keeps the export file column count matching
    # the WQX Web import configuration exactly.
    `Thermal Preservative` = thermal_preservative,
    `Sample Container Type` = sample_container_type,
    `Sample Container Color` = sample_container_color,
    `Chemical Preservative` = chemical_preservative,
    .keep = "unused")

# reduce to template column set
wqx_colnames <- read_excel("other/input/wqx_templates/AWQMS_KWF_Baseline_2021.xlsx",
                            sheet = "KWF Baseline AWQMS Template") %>%
  colnames()
export_dat %<>% select(any_of(wqx_colnames))

# write final results and activities CSV for CDX upload
# na = "" is required: WQX Web's CSV importer expects blank fields for nulls,
# not literal "NA" text, which can trigger an unhandled parser error on import.
write.csv(export_dat, "other/output/wqx_formatted/results_activities.csv", row.names = FALSE, na = "")


# ── Project CSV ──────────────────────────────────────────────────────────────────
# Read from the WQP download, then apply corrections needed for CDX upload:
#   - Keep only the Agency Baseline project row
#   - Update QAPP approval status and approving agency (v3 approved by EPA Region 10, May 2023)
#   - Set ProjectDescriptionText to the project home page URL
#   - Attach the QAPP PDF filename and type
project_upload <- read.csv("other/input/WQX_downloads/project/project.csv") |>
  dplyr::filter(ProjectName == "Agency Baseline") |>
  dplyr::mutate(
    QAPPApprovedIndicator  = "Yes",
    QAPPApprovalAgencyName = "EPA",
    ProjectDescriptionText = "https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/",
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

write.csv(project_upload, "other/output/wqx_formatted/project.csv", row.names = FALSE, na = "")


# ── Station CSV ──────────────────────────────────────────────────────────────────
# Read from the WQP download, filter to KWF Kenai Baseline sites (prefix "KBL"),
# and retain only the columns required by the CDX import configuration.
station_upload <- read.csv("other/input/WQX_downloads/station/station.csv") |>
  dplyr::filter(grepl("KBL", MonitoringLocationName)) |>
  # Strip the org prefix (WQX re-adds it on import); see Monitoring Location ID above
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
  # "GPS-Unspecified" is correct for satellite-based coordinates and is what ADEC uses.
  # Both are corrected here as a safety net for future WQP re-downloads.
  dplyr::mutate(HorizontalCollectionMethodName = dplyr::case_when(
    HorizontalCollectionMethodName %in% c("Interpolation-Satellite", "Unknown") ~ "GPS-Unspecified",
    TRUE ~ HorizontalCollectionMethodName
  ))

write.csv(station_upload, "other/output/wqx_formatted/station.csv", row.names = FALSE, na = "")
