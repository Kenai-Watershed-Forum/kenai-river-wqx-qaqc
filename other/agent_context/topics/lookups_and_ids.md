# Lookup tables and monitoring location IDs

Moved verbatim from AGENTS.md on 2026-11-13. Read when relevant; core memory is AGENTS.md.

## Matching / Lookup Tables

### `wqx_template_matching_table.xlsx`

| Sheet | Key columns |
|----|----|
| `site_coordinates` | `monitoring_location_id` (numeric), lat/long |
| `result_sample_fraction` | `analytical_method` → `result_sample_fraction` |
| `chemical_preservative` | `analytical_method` → preservative |
| `sample_container_type_color` | `analytical_method` → container/color |
| `adec_site_names` | numeric IDs → KBL format + monitoring location names |

### `sgs_site_names_matching_table.xlsx`

Maps SGS sample ID strings → `monitoring_location_id` + `sample_condition` (116 rows as of April 2026; covers 2025 and historical variants). Both `monitoring_location_id` and `sample_condition` are read from this table, no string-based derivation in the QMD.

### `analysis_code_matching_table.xlsx`

Maps SGS method codes → `epa_analysis_id` + `context_code`. Current entries: EPA 200.7, EP200.8 (→ 200.8), SW8260D, SW846 6010D, SM21 4500NO3-F, SM21 4500P-B,E.

### `analytes_list_manual_edit.csv`

Analyte name → abbreviation used in WQX Activity IDs (27 rows as of April 2026). Includes: metals, nutrients, BTEX, TSS, FC, Water Temperature, Specific Conductance, Dissolved Oxygen, Turbidity, pH.

------------------------------------------------------------------------

## Monitoring Location IDs (numeric legacy format)

| ID       | Site                           |
|----------|--------------------------------|
| 10000002 | RM 1.5 – Kenai City Dock       |
| 10000005 | RM 6.5 – Cunningham Park       |
| 10000008 | RM 0 – No Name Creek           |
| 10000015 | RM 10 – Beaver Creek           |
| 10000016 | RM 10.1 – Kenai River          |
| 10000017 | RM 12.5 – Pillars              |
| 10000018 | RM 18 – Poacher's Cove         |
| 10000020 | RM 19 – Slikok Creek           |
| 10000021 | RM 21 – Soldotna Bridge        |
| 10000022 | RM 22 – Soldotna Creek         |
| 10000023 | RM 23 – Swiftwater Park        |
| 10000024 | RM 30 – Funny River            |
| 10000025 | RM 31 – Morgan's Landing       |
| 10000026 | RM 36 – Moose River            |
| 10000027 | RM 40 – Bing's Landing         |
| 10000028 | RM 43 – Upstream of Dow Island |
| 10000029 | RM 44 – Mouth of Killey River  |
| 10000030 | RM 50 – Skilak Lake Outflow    |
| 10000031 | RM 70 – Jim's Landing          |
| 10000032 | RM 74 – Russian River          |
| 10000424 | RM 79.5 – Juneau Creek         |
| 10000425 | RM 82 – Kenai Lake Bridge      |

### ID rule: never include the `KENAI_WQX-` prefix in any ID column

WQX Web prepends `KENAI_WQX-` to IDs on import. WQP downloads already include it, so a value copied from WQP must have the prefix stripped. Upload files use bare IDs: `Monitoring Location ID` = `10000002`, station `MonitoringLocationIdentifier` = `10000002`, Activity IDs built from site code, date, and analyte (no prefix).

On 2026-10-14 the 2021 files were uploaded with prefixed location IDs. WQP then held 22 duplicate stations (`KENAI_WQX-KENAI_WQX-100000xx`) carrying all 830 results; cleanup (delete files, WQX Web delete imports, re-upload) is documented in the report repo at `other/output/epa_wqp_uploads/2026-10-20_doubled_station_cleanup/README.md`.

**Correction (2026-11-05):** an earlier version of this note said the guard already existed. It did not, in this repo or in the report repo (which only strips the prefix). Before 2026-11-05 this repo's `generate_cdx_export.R` also built wrong IDs (site code as Location ID, long site name as Activity ID prefix). Now: `format_wqx.R` builds the final upload IDs once (Location ID = bare numeric ID; Activity ID = `{site_code}-{date}-{abbrev}[-DUP/-Blank]`, site code from `adec_site_names`), `generate_cdx_export.R` uses them unchanged (defensive prefix strip, prefix strip in `station.csv`, `na = ""` on all writes), and `functions/check_cdx_ids.R` `check_cdx_ids(results, station, known_sites)` stops before `results_activities.csv` and `station.csv` are written if: any ID starts with `KENAI_WQX`; a Location ID is not in `adec_site_names`; the station file is not exactly that site list; an Activity ID does not start with its own site code; or an Activity ID has an `NA` component. Valid IDs come from the sheet, never hardcoded. Tests: `tests/test_check_cdx_ids.R` (10 test blocks, 17 expectations, all passing). On the 2025 fixture the guard currently fails on Field Blank rows (`-NA` suffix); that is the Phase C field blank coding item. Notes for future work:

- In the `adec_site_names` sheet of `wqx_template_matching_table.xlsx`, the column `MonitoringLocationIdentifier` holds the site code (`KBL_*`), not a WQP identifier. The script renames it to `site_code`.
- That sheet previously listed `KBL_t_50.0` and `KBL_t_70.0` for IDs 10000030 and 10000031. Both are mainstem sites (`m`), matching the WQP station names `KBL_m_50.0` and `KBL_m_70.0`. Corrected to `m` on 2026-10-20 (same fix in the report repo copy). The 2021 Activity IDs for those two sites (34 rows) previously used `t`; they were deleted from WQP on 2026-10-20 and the re-upload uses `m`.
- 2025 output currently has 116 rows with duplicated Activity IDs (same characteristic, fraction, and method). Not introduced by the ID change; investigate before upload.

------------------------------------------------------------------------

