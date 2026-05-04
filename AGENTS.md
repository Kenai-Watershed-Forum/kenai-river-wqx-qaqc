# Kenai River WQX QAQC — Project Memory

<<<<<<< HEAD
## Repo Relationship

This repo is one of two that together form the full data pipeline for Kenai River Baseline Water Quality Monitoring:

- **`kenai-river-wqx-qaqc`** (this repo):
  https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc
  Prepares annual monitoring data for submission to EPA WQX via CDX. Each year's
  pipeline ingests raw lab EDDs, applies QA/QC, formats data to WQX schema, and
  produces CDX-ready upload files. Once submitted, that data becomes publicly
  available through EPA's Water Quality Portal (WQP).

- **`kenai-river-wqx`** (report repo, source of truth for session_log.md):
  https://github.com/Kenai-Watershed-Forum/kenai-river-wqx
  Hosts the Quarto book that integrates and displays long-term monitoring data.
  Accesses data directly from WQP — it does not hold raw data locally. Applies
  regulatory thresholds, visualizations, and narrative interpretation.

The two repos form a complete pipeline: this qaqc repo submits data to EPA WQX →
EPA publishes to WQP → report repo reads from WQP and displays it. Changes
in either repo — new annual pipelines, CDX corrections, WQX submission status,
regulatory threshold updates — are relevant to both.

**`other/agent_context/session_log.md`** is auto-synced from the report repo and
covers work in both repos. Edit it only from `kenai-river-wqx`.

------------------------------------------------------------------------

=======
>>>>>>> 7830d0047d95da80ebecd14074ad6330a57f6d1c
## Project Purpose

Annual QA/QC pipeline for Kenai River Baseline Water Quality Monitoring data submitted to the U.S. EPA Water Quality Exchange (WQX) via CDX. Managed by Kenai Watershed Forum. Each data year gets its own QMD in `project_year/` (e.g. `project_year/2025.qmd`).

------------------------------------------------------------------------

## Repository Layout

```         
kenai-river-wqx-qaqc/
├── project_year/
│   ├── 2025.qmd                      # Active pipeline (see structure below)
│   ├── 2023.qmd                      # Rebuilt to 2025 template (all chunks eval:false)
│   └── 2014.qmd – 2022.qmd          # Legacy year pipelines (Quarto book chapters)
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
│   │   ├── 2023/
│   │   │   ├── spring_2023_wqx_data/Data/SGS/Revision 1 SGS Data/  (two CSV files: part1, part2)
│   │   │   ├── summer_2023_wqx_data/Data/SGS/Summer 2023 SGS Agency Baseline.xlsx  (Sheet9)
│   │   │   ├── spring_2023_wqx_data/Data/SWWTP/  (FC + TSS xlsx files)
│   │   │   ├── summer_2023_wqx_data/Data/SWWTP/  (FC + TSS xlsx files)
│   │   │   └── misc/  (site name CSVs, lat_long.csv, analytes_list_manual_edit.csv, etc.)
│   │   ├── wqx_templates/
│   │   │   ├── wqx_template_matching_table.xlsx  (5 sheets — see below)
│   │   │   ├── AWQMS_KWF_Baseline_2025.xlsx
│   │   │   ├── sgs_site_names_matching_table.xlsx
│   │   │   ├── analysis_code_matching_table.xlsx
│   │   │   ├── analytes_list_manual_edit.csv
│   │   │   ├── 2025_data_flag_decisions.csv
│   │   │   ├── trip_blank_crews_2025.csv
│   │   │   ├── sample_holding_times.csv
│   │   │   └── wqx_qaqc/wqx_qaqc_info.xlsx  (sheets: field_dup_sites_{year})
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
│   │       │   ├── metals_total_diss/
│   │       │   └── completeness_measures/
│   │       ├── lab_qaqc_data/
│   │       └── misc/
│   └── misc/
│       └── qaqc_repo_transition/
│           └── functions/
│               ├── format_wqx.R           # WQX formatting (sourced in Part B)
│               ├── generate_cdx_export.R  # CDX upload file writer (sourced in Part D)
│               └── apply_qaqc_flags.R     # Joins flag decisions, writes flagged export CSV
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

Maps SGS sample ID strings → `monitoring_location_id` + `sample_condition` (116 rows as of April 2026; covers 2025 and historical variants). Both `monitoring_location_id` and `sample_condition` are read from this table — no string-based derivation in the QMD.

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

**SGS (both seasons)** — Sheet8 of XLSX EDD. `collect_date` and `run_date_time` stored as "MM/DD/YYYY HH:MM" strings. Parsed with `mdy_hm()`, split into date + time columns. `analytical_method`: strip "EP" prefix (EP200.8 → 200.8). `result_sample_fraction`: "Dissolved" / "Unfiltered" assigned from `dissolved == "L"/"."` for EP200.8 only; nutrients/other left NA for `format_wqx.R` lookup. `sample_condition` comes from `sgs_site_names_matching_table.xlsx` (not derived by string). Lab QC rows (MB, LCS, LCSD, MS, MSD, CB, OS, ICV, CCV, CVC, IB, LLQC, QCS, ICB) written to `lab_qaqc_data/` and excluded from `dat`. Spring → `dat_sgs_spring`; Summer → `dat_sgs_summer`; bound → `dat_sgs`.

**Trip blank expansion** — After binding spring + summer SGS, trip blanks labelled "RMx&y trip blank" span two monitoring locations and initially have `monitoring_location_id = NA`. These are expanded to two rows each (one per covered site) via `trip_blank_sites` tribble. Result: `dat_sgs` has 567 rows, zero NA `monitoring_location_id`.

**Fecal Coliform (both seasons)** — Spring: `skip=11`, `Time Sampled` = Excel decimal fraction → `as_hms(t * 86400)`. DUP sites: RM 18 DUP, RM 22 DUP. Summer: `skip=10`, `Time Sampled` = HHMM integer (e.g. 1057 = 10:57) → `as_hms(floor(t/100)*3600 + (t%%100)*60)`. DUP sites: RM 10 DUP, RM 23 DUP. Site lookup inline (`fc_site_lkp` / `fc_site_lkp_sum`). `run_date` hardcoded from file name. Bound → `dat_fc`.

**TSS (both seasons)** — Both seasons read from `Updated_Formatting` sheet. Spring: `KRWF TSS MONITORING 05-01-25.xlsx`, no skip. Site name quirks in file: "RM O" (capital O = zero), "RM22" (no space), "RM22 DUP". `tss_site_lkp` matches these as-is. Summer: `KRWF TSS MONITORING 07-25-25.xls`, `skip=1` (title row). Site names use underscores (RM_0, RM_10_DUP). Normalized in `tss_site_lkp_sum`. `Sample_Time` column is Excel time-only stored as `dttm` with 1899-12-31 date artifact. Parsed with `as_hms(format(Sample_Time, "%H:%M:%S"))`. `collect_date` comes from `spring_sample_date` / `summer_sample_date` (not from file, which shows analysis date). LOD = 0.5 mg/L, LOQ = 1.0 mg/L (placeholders — verify against QAPP). Bound → `dat_tss`.

**YSI / Turbidity** — Single file: `2025 Kenai Agency Baseline YSI ProQuatro and Turbidity Data.xlsx`, Sheet1. Both seasons in one file; season inferred from `Site Depart Date` month. Parameters mapped: Temperature → "Water Temperature" (170.1), Conductivity → "Specific Conductance" (120.1), DO → "Dissolved Oxygen" (360.1), Turbidity → "Turbidity" (180.1), pH → "pH" (150.1). All context_code = "USEPA". Excluded: pH \< 0 or \> 14; DO \> 20 mg/L. Replicates averaged per site-date-parameter; DUP sites kept separate via `ysi_site_lkp`. `result_sample_fraction = "Total"` set in Part A. Produces `dat_ysi`.

**Bind** `dat <- bind_rows(dat_sgs, dat_fc, dat_tss, dat_ysi)` — 865 rows, 23 sites as of April 2026.

### Part B: WQX Formatting

Sources `format_wqx.R`. Joins coordinates, result_sample_fraction (coalesce Part A values with lookup table), chemical preservative, container type. Builds Activity IDs. Writes `2025_kwf_baseline_results_wqx.csv`.

### Part C: QA/QC Checklist

42-question ADEC-based checklist. Flag decisions sourced from `apply_qaqc_flags.R` (Q24 chunk, before completeness measures). Flag decisions CSV (`2025_data_flag_decisions.csv`) filled manually after review — file may be empty (header-only) until flags are determined.

**Q18 note:** dissolved vs. total metals comparison filters to `is.na(sample_condition)` (primary samples only) before `pivot_wider` to avoid list columns from DUP rows.

**Q19 note:** RPD `pivot_wider` uses `mutate(result_value = as.numeric(...))` and `values_fn = \(x) x[1]` to handle character types from CSV read and any duplicate rows.

**Q25/Q26 note:** After `pivot_wider` on `flag`, the "Y" column will be absent when no results are flagged yet. Guard with `if (!"Y" %in% names(df)) df$Y <- NA_integer_` before renaming.

### Part D: CDX Export

Sources `generate_cdx_export.R`. Guarded by `file.exists()` check on WQP project file — prints a message and skips if WQP downloads not yet present. Requires `other/input/WQX_downloads/` to be populated manually from WQP before running.

------------------------------------------------------------------------

## `format_wqx.R` Key Behaviors

- `result_sample_fraction`: Part A values take precedence (coalesce); lookup table fills gaps. TSS always → "Suspended"; FC always → "None" (overrides applied after coalesce).
- `monitoring_location_id` coerced to character before join (was numeric in site_coords sheet).
- Activity ID: `{monitoring_location_id}-{collect_date}-{analyte_abbreviation}[-DUP/-Blank]`
- Field Duplicate Activity Type: "Quality Control Field Replicate Msr/Obs"

## `apply_qaqc_flags.R` Key Behaviors

- Reads `cfg$wqx_intermediate_path`, joins `cfg$flag_decisions_path`, writes `cfg$flagged_export_path`.
- `flag_decisions` read with explicit `colClasses` to handle empty (header-only) CSV without type coercion to logical.
- `monitoring_location_id` coerced to character in `export_dat` before join.
- Leaves `export_dat` in global environment for downstream Q25/Q26 and Part D.

------------------------------------------------------------------------

## 2023.qmd Status

Rebuilt to match 2025 template structure (YAML at top, `date: today`, Part A/B/C/D layout, `here::here()` paths). All 22 code chunks are `eval: false` — pipeline not yet ported to new template; document renders as book chapter without executing code.

**2023-specific data notes (preserved in Part A notes block):** - Spring sampling: 2023-05-02. Summer: 2023-07-18. - SGS spring: two CSV files (Revision 1, part1 + part2). Summer: XLSX Sheet9. - Summer 2023: EPA 200.8 used for both total AND dissolved metals (no 200.7). `ANALYSIS_GROUP` column distinguishes total vs. dissolved. - Spring receipt date corrected: EDD shows 05/09/2023; actual receipt was 05/04/2023 08:51. - Spring collect date corrected: some samples show 2023-05-03 (COC typo) → 2023-05-02. - SW846 6010D rows filtered out (no reportable values). - Spring 2023: RM 10.1 not visited (low water); RM 40 FC sample spilled on-site. - Dissolved Cu and Zn flagged: dissolved \> total in 32 of 52 pairs; two spring dissolved Zn method blanks above LOQ. - TSS LOD = 0.31 mg/L. Spring receipt times from COC ReadMe: RM_1.5 = 15:30, others = 12:44; summer = 12:25.

------------------------------------------------------------------------

## Known Issues / Flags

- **EPA ETL blocked:** 2021 data (835 records) cannot be re-uploaded pending EPA fix.
- **WQP downloads:** `other/input/WQX_downloads/` must be populated manually before Part D runs. Part D is now guarded with a `file.exists()` check and will print a message instead of erroring.
- **TSS LOD/LOQ:** 0.5 / 1.0 mg/L are placeholders. Verify against current QAPP.
- **Summer FC run_date:** hardcoded as 7/24/2025; verify against SWWTP lab report.
- **YSI site mapping:** any site names not in `ysi_site_lkp` will print a warning and be excluded.
- **`date: today`:** Both `index.qmd` and `project_year/2025.qmd` use `date: today` (Quarto native). Do not use `` `r Sys.Date()` `` in YAML — it will not render correctly.

------------------------------------------------------------------------

## QA/QC Checklist Question Numbering and Wording

The canonical reference for all 42 QA/QC question texts is `appendix_a.qmd` in the `kenai-river-wqx` repo: https://github.com/Kenai-Watershed-Forum/kenai-river-wqx/blob/main/chapters/appendix_a.qmd

All three active files — `project_year/2025.qmd`, `project_year/2023.qmd`, and `other/misc/qaqc_repo_transition/templates/pipeline_template.qmd` — have been synchronized to that reference (April 2026). When adding a new year QMD, copy question text from `pipeline_template.qmd`, which is kept in sync.

Notable Q2/Q3 wording (most commonly diverged in past): - **Q2:** "Were there any deviations from the sampling plan?" - **Q3:** "Were field duplicates, blanks, and/or other QC samples collected as planned?"

------------------------------------------------------------------------

## Implementation Status (as of April 2026)

| Task | Status |
|----|----|
| Phase 0: Infrastructure (templates, matching tables, output dirs) | ✅ Done |
| Phase 1: 2025.qmd — spring SGS/FC/TSS | ✅ Done |
| Phase 1: 2025.qmd — summer SGS/FC/TSS | ✅ Done |
| Phase 1: 2025.qmd — YSI/turbidity | ✅ Done |
| Phase 1: Spring TSS migrated to Updated_Formatting XLSX | ✅ Done |
| Phase 1: Trip blank expansion (dat_sgs 567 rows, 0 NA loc IDs) | ✅ Done |
| Phase 1: 2025.qmd render errors fixed (Q18, Q19, Q25, Q26, Part D guard) | ✅ Done |
| QA/QC checklist questions standardized across 2025.qmd, 2023.qmd, pipeline_template.qmd | ✅ Done |
| Phase 2: 2024.qmd | ⏳ Pending |
| Phase 3: 2023.qmd (skeleton rebuilt; pipeline code still eval:false) | 🔄 In progress |
| Phase 4: 2022.qmd | ⏳ Pending |
| Phase 5: 2021 status (ETL blocked) | ⏳ Pending |
| Phase 6: 2020–2014 pipelines | ⏳ Pending |
| Phase 7: Characteristic name audit & WQX standardization | ⏳ Pending |
