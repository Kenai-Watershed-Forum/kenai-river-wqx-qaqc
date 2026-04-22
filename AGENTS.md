# Kenai River WQX QAQC — Project Memory

## Project Purpose

Annual QA/QC pipeline for Kenai River Baseline Water Quality Monitoring data submitted to the U.S. EPA Water Quality Exchange (WQX) via CDX. Managed by Kenai Watershed Forum. Each data year gets its own QMD in the repo root (e.g. `2025.qmd`).

------------------------------------------------------------------------

## Repository Layout

```         
kenai-river-wqx-qaqc/
├── project_year/
│   ├── 2025.qmd                      # Active pipeline (see structure below)
│   └── 2014.qmd – 2023.qmd          # Legacy year pipelines (Quarto book chapters)
├── other/
│   ├── input/
│   │   ├── 2025/
│   │   │   ├── spring_2025_wqx_data/
│   │   │   │   ├── SGS/  spring_2025_kenai_baseline_sgs_results.xlsx  (Sheet8)
│   │   │   │   └── SWWTP/
│   │   │   │       ├── KRWF Fecal 04-30-25.xls          (skip=11)
│   │   │   │       ├── KRWF TSS MONITORING 05-01-25.xls  (original, kept for reference)
│   │   │   │       └── KRWF TSS MONITORING 05-01-25.xlsx (Updated_Formatting sheet — USE THIS)
│   │   │   ├── summer_2025_wqx_data/
│   │   │   │   ├── SGS/  summer_2025_kenai_baseline_results_sgs.xlsx  (Sheet8)
│   │   │   │   └── SWWTP/
│   │   │   │       ├── KRWF Fecal 07-23-25.xls           (skip=10)
│   │   │   │       └── KRWF TSS MONITORING 07-25-25.xls  (Updated_Formatting sheet, skip=1)
│   │   │   └── 2025 Kenai Agency Baseline YSI ProQuatro and Turbidity Data.xlsx
│   │   ├── wqx_templates/
│   │   │   ├── wqx_template_matching_table.xlsx  (5 sheets — see below)
│   │   │   ├── AWQMS_KWF_Baseline_2025.xlsx
│   │   │   ├── sgs_site_names_matching_table.xlsx
│   │   │   ├── analysis_code_matching_table.xlsx
│   │   │   ├── analytes_list_manual_edit.csv
│   │   │   ├── 2025_data_flag_decisions.csv
│   │   │   ├── trip_blank_crews_2025.csv
│   │   │   └── sample_holding_times.csv
│   │   ├── WQX_downloads/            # Must be downloaded manually from WQP before Part D
│   │   └── regulatory_limits/
│   ├── output/                              # Final WQX/CDX upload-ready files
│   │   ├── 2025_kwf_baseline_results_wqx.csv
│   │   ├── 2025_export_data_flagged.csv
│   │   ├── results_activities.csv
│   │   ├── project.csv
│   │   ├── station.csv
│   │   └── intermediate/                    # Working/QA outputs (not for upload)
│   │       ├── 2025/                        # Year-specific QA working files
│   │       ├── field_qa_qc_data/
│   │       ├── lab_qaqc_data/
│   │       └── misc/
│   └── misc/
│       └── qaqc_repo_transition/
│           └── functions/
│               ├── format_wqx.R         # WQX formatting (sourced in Part B)
│               └── generate_cdx_export.R
```

------------------------------------------------------------------------

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

Maps SGS sample ID strings → `monitoring_location_id` (107 rows; covers 2025 and historical variants).

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

------------------------------------------------------------------------

## 2025.qmd Pipeline Structure

### Year Configuration chunk

Sets `spring_sample_date = "4/30/2025"`, `summer_sample_date = "7/23/2025"`, paths, and `cfg` list. **This is the only block that changes between years.** All paths use `here::here()` so the document works correctly from `project_year/` regardless of the RStudio "Evaluate chunks in directory" setting.

### Part A: Data Ingestion → produces `dat`

**SGS (both seasons)** - Sheet8 of XLSX EDD. `collect_date` and `run_date_time` stored as "MM/DD/YYYY HH:MM" strings. - Parsed with `mdy_hm()`, split into date + time columns. - `analytical_method`: strip "EP" prefix (EP200.8 → 200.8). - `result_sample_fraction`: "Dissolved" / "Unfiltered" assigned from `dissolved == "L"/"."` for EP200.8 only; nutrients/other left NA for `format_wqx.R` lookup. - `sample_condition` derived from `sample_id` string (DUP, Field Blank, Trip Blank, Method Blank). - Joined to `analysis_code_matching_table.xlsx` and `sgs_site_names_matching_table.xlsx`. - Lab QC rows (MB, LCS, LCSD, MS, MSD, CB, OS, ICV, CCV, LLQC, QCS, ICB) written to `lab_qaqc_data/` and excluded from `dat`. - Spring → `dat_sgs_spring`; Summer → `dat_sgs_summer`; bound → `dat_sgs`.

**Fecal Coliform (both seasons)** - Spring: `skip=11`, `Time Sampled` = Excel decimal fraction → `as_hms(t * 86400)`. DUP sites: RM 18 DUP, RM 22 DUP. - Summer: `skip=10`, `Time Sampled` = HHMM integer (e.g. 1057 = 10:57) → `as_hms(floor(t/100)*3600 + (t%%100)*60)`. DUP sites: RM 10 DUP, RM 23 DUP. - Site lookup inline (`fc_site_lkp` / `fc_site_lkp_sum`). `run_date` hardcoded from file name. - Bound → `dat_fc`.

**TSS (both seasons)** - Both seasons read from `Updated_Formatting` sheet. - Spring: `KRWF TSS MONITORING 05-01-25.xlsx`, no skip. Site name quirks in file: "RM O" (capital O = zero), "RM22" (no space), "RM22 DUP". `tss_site_lkp` matches these as-is. - Summer: `KRWF TSS MONITORING 07-25-25.xls`, `skip=1` (title row). Site names use underscores (RM_0, RM_10_DUP). Normalized in `tss_site_lkp_sum`. - `Sample_Time` column is Excel time-only stored as `dttm` with 1899-12-31 date artifact. Parsed with `as_hms(format(Sample_Time, "%H:%M:%S"))`. - `collect_date` comes from `spring_sample_date` / `summer_sample_date` (not from file, which shows analysis date). - LOD = 0.5 mg/L, LOQ = 1.0 mg/L (placeholders — verify against QAPP). - Bound → `dat_tss`.

**YSI / Turbidity** - Single file: `2025 Kenai Agency Baseline YSI ProQuatro and Turbidity Data.xlsx`, Sheet1. - Both seasons in one file; season inferred from `Site Depart Date` month. - Columns used: `Site Name`, `Site Depart Date`, `Collection Time (HH:MM)`, `Parameter`, `Value`. - Parameters mapped: Temperature → "Water Temperature" (170.1), Conductivity → "Specific Conductance" (120.1), DO → "Dissolved Oxygen" (360.1), Turbidity → "Turbidity" (180.1), pH → "pH" (150.1). All context_code = "USEPA". - Excluded: pH \< 0 or \> 14; DO \> 20 mg/L (instrument errors flagged in field notes). - Replicates averaged per site-date-parameter; DUP sites kept separate via `ysi_site_lkp`. - `result_sample_fraction = "Total"` set in Part A (preserved by `format_wqx.R` coalesce logic). - Produces `dat_ysi`.

**Bind** `dat <- bind_rows(dat_sgs, dat_fc, dat_tss, dat_ysi)`

### Part B: WQX Formatting

Sources `format_wqx.R`. Joins coordinates, result_sample_fraction (coalesce Part A values with lookup table), chemical preservative, container type. Builds Activity IDs. Writes `2025_kwf_baseline_results_wqx.csv`.

### Part C: QA/QC Checklist

42-question ADEC-based checklist. Flag decisions read from `2025_data_flag_decisions.csv` (fill manually after review).

### Part D: CDX Export

Sources `generate_cdx_export.R`. Requires WQP downloads in `other/input/WQX_downloads/` (must be downloaded manually before running).

------------------------------------------------------------------------

## `format_wqx.R` Key Behaviors

- `result_sample_fraction`: Part A values take precedence (coalesce); lookup table fills gaps. TSS always → "Suspended"; FC always → "None" (overrides applied after coalesce).
- Activity ID: `{monitoring_location_id}-{collect_date}-{analyte_abbreviation}[-DUP/-Blank]`
- Field Duplicate Activity Type: "Quality Control Field Replicate Msr/Obs"

------------------------------------------------------------------------

## Known Issues / Flags

- **EPA ETL blocked:** 2021 data (835 records) cannot be re-uploaded pending EPA fix.
- **WQP downloads:** `other/input/WQX_downloads/` must be populated manually before Part D runs.
- **TSS LOD/LOQ:** 0.5 / 1.0 mg/L are placeholders. Verify against current QAPP.
- **Summer FC run_date:** hardcoded as 7/24/2025; verify against SWWTP lab report.
- **YSI site mapping:** any site names not in `ysi_site_lkp` will print a warning and be excluded.

------------------------------------------------------------------------

## Implementation Status (as of April 2026)

| Task | Status |
|----|----|
| Phase 0: Infrastructure (templates, matching tables, output dirs) | ✅ Done |
| Phase 1: 2025.qmd — spring SGS/FC/TSS | ✅ Done |
| Phase 1: 2025.qmd — summer SGS/FC/TSS | ✅ Done |
| Phase 1: 2025.qmd — YSI/turbidity | ✅ Done |
| Phase 1: Spring TSS migrated to Updated_Formatting XLSX | ✅ Done |
| Phase 2: 2024.qmd | ⏳ Pending |
| Phase 3: 2023.qmd (rebuild from template) | ⏳ Pending |
| Phase 4: 2022.qmd | ⏳ Pending |
| Phase 5: 2021 status (ETL blocked) | ⏳ Pending |
| Phase 6: 2020–2014 pipelines | ⏳ Pending |
| Phase 7: Characteristic name audit & WQX standardization | ⏳ Pending |
