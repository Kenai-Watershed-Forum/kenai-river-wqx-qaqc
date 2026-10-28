# Kenai River WQX QAQC: Project Memory

## Repo Relationship and Purpose

Two repos form the Kenai River Baseline Water Quality Monitoring pipeline (Kenai Watershed Forum, KWF):

- **`kenai-river-wqx-qaqc`** (this repo, https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc): a **single-year template**. It takes any one project year's raw lab files through ingestion, QA/QC, WQX formatting and CDX export, and produces a draft (preliminary) report like the 2025 one. It repeats the process that `appendix_a` documents in the report repo.
- **`kenai-river-wqx`** (report repo, https://github.com/Kenai-Watershed-Forum/kenai-river-wqx, stays active and standalone): a Quarto book on long-term trends and observations. It reads data from EPA's Water Quality Portal (WQP), not from this repo. Its `chapters/appendix_a.qmd` is a worked example of taking one annual dataset through QA/QC and is the canonical text for the 42 QA/QC questions.

Flow: this repo submits data to EPA WQX via CDX, EPA publishes to WQP, the report repo reads from WQP.

**Divergence to be aware of:** the report repo keeps its own copies of the ingest and format code in `functions/appendix_a_scripts/`. They differ from `functions/` here (copies are in `other/misc/report_repo_scripts/` for comparison). How the two should relate (frozen example versus this repo as source of truth) is undecided. Fixes made in one are not automatically in the other.

**`other/agent_context/session_log.md`** is auto-synced from the report repo and covers both repos. Edit it only from `kenai-river-wqx`.

Design record: `other/agent_context/plans/2026-10-28-qaqc-repo-as-parameterized-template.md` (rev. 2, approved 2026-10-28; read it before making architectural changes). It supersedes Phases 2 and 3 of `2026-10-14-1300-per-year-repo-architecture-migration.md` (Phases 0 and 1 of that plan stand). This repo is **not** archived.

## Working Model

One working year at a time: that year's raw files, config, outputs and report, plus shared code and reference data. Years are developed **2025 first, then backwards** (2024, 2023, ...). There is no cross-year regression baseline; intake and output handling are customized per year. A finished year is archived as a GitHub Release (`raw-data-{year}`, public, contains everything the labs sent) and removed from the working tree. See `README.md` for the new-year and archive checklists, and `functions/fetch_raw_data.R` and `functions/archive_year.R`.

**Phases** (each ends with a check-in; present decisions to the user one at a time):

| Phase | Content | Status |
|----|----|----|
| A | Contract, layout, archive mechanics | Largely done. Remaining: reconcile function copies with the report repo's scripts |
| B | 2025 ingestion as per-lab functions with a season argument; surrogates to `lab_qc_dat`; drop `matrix` and `result_detection_condition`; `mg/L` guard; contract-check function | Next |
| C | `qaqc.qmd` Parts B-D; field blank coding; surrogate check in Q20; write `assessed_results.csv` and `lab_qc_assessed.csv` | Pending |
| D | `prelim_report.qmd` generalized from `other/documents/preliminary_report_2025/2025_preliminary_summary.qmd`; its setup chunk (about lines 200-530) re-ingests raw data and is replaced by reading `assessed_results.csv` | Pending |
| E | Archive 2025, promotion review, reset for 2024, update both repos' docs | Pending |

Key decisions are in the plan. The ones that most affect day-to-day work: `dat` has 24 columns (see `reference/dat_contract.md`); field blanks use Activity Type `Quality Control Sample-Field Blank` and Activity ID suffix `FB`; `lab_qc_dat` holds all 14 SGS lab QC codes plus any `(surr)` analyte and never flows to the WQX export; `prelim_report.qmd` reads only `output/assessed_results.csv`, `config.yml` and reference data.

## Current Layout

The pipeline is mid-migration. Today the working pipeline is `project_year/2025.qmd`; the target is `qaqc.qmd` plus `prelim_report.qmd`.

```
kenai-river-wqx-qaqc/
├── config.yml                  # Working-year settings (dates, lab file paths/sheets/skip, relative to data/)
├── data/                       # Raw lab files, gitignored; only {year}.sha256 is committed
├── functions/                  # format_wqx.R, apply_qaqc_flags.R, generate_cdx_export.R,
│                               # fetch_raw_data.R, archive_year.R
├── reference/
│   ├── dat_contract.md         # 24-column contract for `dat` (not yet enforced in code)
│   ├── wqx_templates/          # Shared lookup tables (see below)
│   ├── master_reg_limits.xlsx  # Copied from report repo (needed by prelim_report.qmd)
│   ├── baseline_sites.csv      # Copied from report repo
│   └── formatted_reg_vals/calculated_metals_reg_vals.csv   # Snapshot from report repo
├── year_files/                 # Working-year-specific: {year}_data_flag_decisions.csv,
│                               # AWQMS_KWF_Baseline_{year}.xlsx, trip_blank_crews_{year}.csv
├── output/                     # Working-year outputs, committed while in progress:
│                               # results_activities.csv, {year}_kwf_baseline_results_wqx.csv,
│                               # {year}_export_data_flagged.csv, intermediate/ (QA working files)
├── project_year/               # 2025.qmd active; 2023.qmd skeleton; 2014-2022 legacy
├── other/
│   ├── agent_context/          # session_log.md (synced), plans/ (design record)
│   ├── documents/              # md/ (QAPP, CALM, 18 AAC 70, SGS DL/LOD/LOQ, MOU), wqx_documents/
│   │                           # (review checklists), preliminary_report_2025/ (source for Phase D), images/
│   ├── misc/report_repo_scripts/   # Report repo's ingest/format/export scripts, for Phase B reference
│   ├── misc/qaqc_repo_transition/  # OLD function copies and 2021 ingest scripts (stale; do not edit)
│   ├── input/                  # Legacy per-year inputs 2014-2024; WQX_downloads/ (manual WQP downloads for Part D)
│   └── output/                 # Legacy 2023 and example outputs
├── _quarto.yml                 # Plain project (type: default, output-dir output/report); legacy QMDs are not rendered as a book
└── README.md                   # Layout, new-year checklist, archive checklist, restore
```

Governance PDFs (QAPP, CALM, 18 AAC 70, etc.) stay in the report repo's `other/agent_context/`; text versions are in `other/documents/md/` here. WQX Web import configuration (Uid 9481) walkthrough is in the report repo's `chapters/cdx_upload.qmd`; the exported config is not yet committed anywhere.

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

`functions/generate_cdx_export.R` now builds bare IDs and stops with an error before writing if any ID column starts with `KENAI_WQX`, if a location ID is not one of the 22 known IDs above, or if `station.csv` is not exactly those 22 stations. Notes for future work:

- In the `adec_site_names` sheet of `wqx_template_matching_table.xlsx`, the column `MonitoringLocationIdentifier` holds the site code (`KBL_*`), not a WQP identifier. The script renames it to `site_code`.
- That sheet previously listed `KBL_t_50.0` and `KBL_t_70.0` for IDs 10000030 and 10000031. Both are mainstem sites (`m`), matching the WQP station names `KBL_m_50.0` and `KBL_m_70.0`. Corrected to `m` on 2026-10-20 (same fix in the report repo copy). The 2021 Activity IDs for those two sites (34 rows) previously used `t`; they were deleted from WQP on 2026-10-20 and the re-upload uses `m`.
- 2025 output currently has 116 rows with duplicated Activity IDs (same characteristic, fraction, and method). Not introduced by the ID change; investigate before upload.

------------------------------------------------------------------------

## 2025.qmd Pipeline Structure

### Year Configuration chunk

Sets `spring_sample_date = "4/30/2025"`, `summer_sample_date = "7/23/2025"`, paths, and `cfg` list. **This is the only block that changes between years.** All paths use `here::here()` so the document works correctly from `project_year/` regardless of the RStudio "Evaluate chunks in directory" setting.

### Part A: Data Ingestion → produces `dat`

**SGS (both seasons)**, Sheet8 of XLSX EDD. `collect_date` and `run_date_time` stored as "MM/DD/YYYY HH:MM" strings. Parsed with `mdy_hm()`, split into date + time columns. `analytical_method`: strip "EP" prefix (EP200.8 → 200.8). `result_sample_fraction`: "Dissolved" / "Unfiltered" assigned from `dissolved == "L"/"."` for EP200.8 only; nutrients/other left NA for `format_wqx.R` lookup. `sample_condition` comes from `sgs_site_names_matching_table.xlsx` (not derived by string). Lab QC rows (MB, LCS, LCSD, MS, MSD, CB, OS, ICV, CCV, CVC, IB, LLQC, QCS, ICB) written to `lab_qaqc_data/` and excluded from `dat`. Spring → `dat_sgs_spring`; Summer → `dat_sgs_summer`; bound → `dat_sgs`.

**Trip blank expansion**, After binding spring + summer SGS, trip blanks labelled "RMx&y trip blank" span two monitoring locations and initially have `monitoring_location_id = NA`. These are expanded to two rows each (one per covered site) via `trip_blank_sites` tribble. Result: `dat_sgs` has 567 rows, zero NA `monitoring_location_id`.

**Fecal Coliform (both seasons)**, Spring: `skip=11`, `Time Sampled` = Excel decimal fraction → `as_hms(t * 86400)`. DUP sites: RM 18 DUP, RM 22 DUP. Summer: `skip=10`, `Time Sampled` = HHMM integer (e.g. 1057 = 10:57) → `as_hms(floor(t/100)*3600 + (t%%100)*60)`. DUP sites: RM 10 DUP, RM 23 DUP. Site lookup inline (`fc_site_lkp` / `fc_site_lkp_sum`). `run_date` hardcoded from file name. Bound → `dat_fc`.

**TSS (both seasons)**, Both seasons read from `Updated_Formatting` sheet. Spring: `KRWF TSS MONITORING 05-01-25.xlsx`, no skip. Site name quirks in file: "RM O" (capital O = zero), "RM22" (no space), "RM22 DUP". `tss_site_lkp` matches these as-is. Summer: `KRWF TSS MONITORING 07-25-25.xls`, `skip=1` (title row). Site names use underscores (RM_0, RM_10_DUP). Normalized in `tss_site_lkp_sum`. `Sample_Time` column is Excel time-only stored as `dttm` with 1899-12-31 date artifact. Parsed with `as_hms(format(Sample_Time, "%H:%M:%S"))`. `collect_date` comes from `spring_sample_date` / `summer_sample_date` (not from file, which shows analysis date). LOD = 0.5 mg/L, LOQ = 1.0 mg/L (placeholders, verify against QAPP). Bound → `dat_tss`.

**YSI / Turbidity**, Single file: `2025 Kenai Agency Baseline YSI ProQuatro and Turbidity Data.xlsx`, Sheet1. Both seasons in one file; season inferred from `Site Depart Date` month. Parameters mapped: Temperature → "Water Temperature" (170.1), Conductivity → "Specific Conductance" (120.1), DO → "Dissolved Oxygen" (360.1), Turbidity → "Turbidity" (180.1), pH → "pH" (150.1). All context_code = "USEPA". Excluded: pH \< 0 or \> 14; DO \> 20 mg/L. Replicates averaged per site-date-parameter; DUP sites kept separate via `ysi_site_lkp`. `result_sample_fraction = "Total"` set in Part A. Produces `dat_ysi`.

**Bind** `dat <- bind_rows(dat_sgs, dat_fc, dat_tss, dat_ysi)`, 883 rows, 22 sites, 27 analytes per `reference/dat_contract.md` (Oct 2026; earlier notes said 865 rows / 23 sites).

### Part B: WQX Formatting

Sources `format_wqx.R`. Joins coordinates, result_sample_fraction (coalesce Part A values with lookup table), chemical preservative, container type. Builds Activity IDs. Writes `2025_kwf_baseline_results_wqx.csv`.

### Part C: QA/QC Checklist

42-question ADEC-based checklist. Flag decisions sourced from `apply_qaqc_flags.R` (Q24 chunk, before completeness measures). Flag decisions CSV (`2025_data_flag_decisions.csv`) filled manually after review, file may be empty (header-only) until flags are determined.

**Q18 note:** dissolved vs. total metals comparison filters to `is.na(sample_condition)` (primary samples only) before `pivot_wider` to avoid list columns from DUP rows.

**Q19 note:** RPD `pivot_wider` uses `mutate(result_value = as.numeric(...))` and `values_fn = \(x) x[1]` to handle character types from CSV read and any duplicate rows.

**Q25/Q26 note:** After `pivot_wider` on `flag`, the "Y" column will be absent when no results are flagged yet. Guard with `if (!"Y" %in% names(df)) df$Y <- NA_integer_` before renaming.

### Part D: CDX Export

Sources `generate_cdx_export.R`. Guarded by `file.exists()` check on WQP project file, prints a message and skips if WQP downloads not yet present. Requires `other/input/WQX_downloads/` to be populated manually from WQP before running.

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

Rebuilt to match 2025 template structure (YAML at top, `date: today`, Part A/B/C/D layout, `here::here()` paths). All 22 code chunks are `eval: false`, pipeline not yet ported to new template; document renders as book chapter without executing code.

**2023-specific data notes (preserved in Part A notes block):** - Spring sampling: 2023-05-02. Summer: 2023-07-18. - SGS spring: two CSV files (Revision 1, part1 + part2). Summer: XLSX Sheet9. - Summer 2023: EPA 200.8 used for both total AND dissolved metals (no 200.7). `ANALYSIS_GROUP` column distinguishes total vs. dissolved. - Spring receipt date corrected: EDD shows 05/09/2023; actual receipt was 05/04/2023 08:51. - Spring collect date corrected: some samples show 2023-05-03 (COC typo) → 2023-05-02. - SW846 6010D rows filtered out (no reportable values). - Spring 2023: RM 10.1 not visited (low water); RM 40 FC sample spilled on-site. - Dissolved Cu and Zn flagged: dissolved \> total in 32 of 52 pairs; two spring dissolved Zn method blanks above LOQ. - TSS LOD = 0.31 mg/L. Spring receipt times from COC ReadMe: RM_1.5 = 15:30, others = 12:44; summer = 12:25.

------------------------------------------------------------------------

## Known Issues / Flags

Contract violations found in the 2025 data (to be fixed in Phases B and C):

- **Units case:** 48 SWWTP TSS rows carry `mg/l`; `2025.qmd` hardcodes it. The report repo's `ingest_tss.R` was fixed 2026-10-20 but this repo's copy was not.
- **Surrogates leaking:** 24 BTEX surrogate recovery rows (units `%`) are in `dat` and in the 2025 WQX export as `Field Msr/Obs`. They belong in `lab_qc_dat`.
- **Dead branch:** `format_wqx.R` tests `sample_condition == "Blank"`, but ingest emits `Trip Blank` and `Field Blank`.
- **Field blanks not coded:** 44 `Field Blank` rows in `dat`; the export has no field-blank Activity Type, and 56 Activity IDs end in `-NA`.
- **Duplicate Activity IDs:** 116 rows in the 2025 output share an Activity ID with the same characteristic, fraction and method. Not caused by the ID change; investigate before upload.
- **Duplicated spring and summer ingest chunks** (supports per-lab functions with a season argument).

Other:

- **Function copies differ** from the report repo's `functions/appendix_a_scripts/` (large diffs in `format_wqx.R` and `generate_cdx_export.R`). Reconcile in Phase B; the report repo's `generate_cdx_export.R` has the bare-ID guard and the `na = ""` fix.
- **WQP downloads:** `other/input/WQX_downloads/` must be populated manually before Part D runs; Part D prints a message and skips if absent.
- **2021 on WQP:** corrected 2021 files were re-uploaded 2026-10-25 (CDX datasets 117639, 117640, 117643, all Completed at CDX). A live query on 2026-10-28 showed 0 rows for 2021 and no doubled-prefix stations; recheck about Nov 1-3. Tracked in the report repo.
- **TSS LOD/LOQ:** 0.5 / 1.0 mg/L are placeholders; verify against the current QAPP.
- **Summer FC run_date:** hardcoded as 7/24/2025; verify against the SWWTP lab report.
- **YSI site mapping:** site names not in `ysi_site_lkp` print a warning and are excluded.
- **`date: today`:** use Quarto's native `date: today` in YAML, not an inline R `Sys.Date()` call.

------------------------------------------------------------------------

## QA/QC Checklist Question Numbering and Wording

The canonical reference for all 42 QA/QC question texts is `appendix_a.qmd` in the `kenai-river-wqx` repo: https://github.com/Kenai-Watershed-Forum/kenai-river-wqx/blob/main/chapters/appendix_a.qmd

All three active files, `project_year/2025.qmd`, `project_year/2023.qmd`, and `other/misc/qaqc_repo_transition/templates/pipeline_template.qmd`, have been synchronized to that reference (April 2026). When adding a new year QMD, copy question text from `pipeline_template.qmd`, which is kept in sync.

Notable Q2/Q3 wording (most commonly diverged in past): - **Q2:** "Were there any deviations from the sampling plan?" - **Q3:** "Were field duplicates, blanks, and/or other QC samples collected as planned?"

------------------------------------------------------------------------

------------------------------------------------------------------------

## Report-Repo Items (tracked in `kenai-river-wqx`, not done here)

2021 WQP verification, cache refresh and book render; trend analysis rollout; characteristic name audit (Task 1b); CALM 5-year sample counts (Task 1c); `review_needed` verification in `master_reg_limits.xlsx` (Task 5); committing WQX Web config Uid 9481. See that repo's `AGENTS.md`.

## Style Preferences

- No em-dashes; use a colon, comma, semicolon or parentheses. Minimal AI-writing tropes.
- All ingest scripts must emit `mg/L` (WQX capitalization).
- Do not interpret trends in text.
- Before implementing code, explain what it does and why, and wait for confirmation. Present decisions one at a time, not as a batch.
