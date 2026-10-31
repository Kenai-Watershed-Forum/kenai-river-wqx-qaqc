# Kenai River WQX QAQC: Project Memory

## Repo Relationship and Purpose

Two repos form the Kenai River Baseline Water Quality Monitoring pipeline (Kenai Watershed Forum, KWF):

- **`kenai-river-wqx-qaqc`** (this repo, https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc): a **single-year template**. It takes any one project year's raw lab files through ingestion, QA/QC, WQX formatting and CDX export, and produces a draft (preliminary) report like the 2025 one. It repeats the process that `appendix_a` documents in the report repo.
- **`kenai-river-wqx`** (report repo, https://github.com/Kenai-Watershed-Forum/kenai-river-wqx, stays active and standalone): a Quarto book on long-term trends and observations. It reads data from EPA's Water Quality Portal (WQP), not from this repo. Its `chapters/appendix_a.qmd` is a worked example of taking one annual dataset through QA/QC and is the canonical text for the 42 QA/QC questions.

Flow: this repo submits data to EPA WQX via CDX, EPA publishes to WQP, the report repo reads from WQP.

**Divergence to be aware of:** the report repo keeps its own copies of the ingest and format code in `functions/appendix_a_scripts/`. They differ from `functions/` here (copies are in `other/misc/report_repo_scripts/` for comparison). How the two should relate (frozen example versus this repo as source of truth) is undecided. Fixes made in one are not automatically in the other.

**`other/agent_context/session_log.md`** is auto-synced from the report repo and covers both repos. Edit it only from `kenai-river-wqx`.

Design record: `other/agent_context/plans/2026-10-28-qaqc-repo-as-parameterized-template.md` (rev. 2, approved 2026-10-28; read it before making architectural changes). It supersedes Phases 2 and 3 of `2026-10-14-1300-per-year-repo-architecture-migration.md` (Phases 0 and 1 of that plan stand). This repo is **not** archived. **Note (2026-10-28, later in the day): the plan file's archive/fetch/checksum content and its item 13 (plain `type: default`) are out of date; the blank-template model below supersedes them. The plan file has not yet been revised.**

## Working Model

**This repo is being built into a blank, robust template that works for any year.** It is not a repo that processes 2025. For each project year the user copies the template into a **new GitHub repo** and puts only that year's raw lab files in `data/` (committed there; one year per repo). 2025 is the test fixture used to refine the template. The template must run the full QA/QC process for an annual dataset and draft the preliminary report.

- Function code must contain nothing year-specific. Year-specific values live only in `config.yml`, `year_files/` and reference tables.
- The 2025 data and outputs are removed at the end, leaving placeholders, `data/.gitkeep`, and a README new-year checklist. Mark the repo a GitHub template repository. A trimmed example folder (suggested `example_2025/`, contents undecided; raw lab files may not be wanted) stays in the template.
- Also removed at the end: legacy `project_year/2014-2024` and `other/input/<year>` folders.
- **Archive and fetch mechanics were deleted** (2026-10-28): `functions/fetch_raw_data.R`, `functions/archive_year.R`, `data/.gitignore`, the `.sha256` files and the `raw-data-{year}` releases are no longer part of the design. `README.md` and the plan file still describe them and need rewriting.
- The "2025 first, then backwards" sequence and the promotion review no longer apply.

**Phases** (each ends with a check-in; present decisions to the user one at a time):

| Phase | Content | Status |
|----|----|----|
| A | Contract, layout | Largely done. Remaining: reconcile function copies with the report repo's scripts (deferred until code settles) |
| B | Ingestion as per-lab functions with a season argument; surrogates to `lab_qc_dat`; drop `matrix` and `result_detection_condition`; `mg/L` guard; contract-check function | In progress. `ingest_sgs.R` and `ingest_fc.R` done and verified; `ingest_tss.R` done and verified (2026-10-28); `clock_utils.R` split out (2026-10-28); `ingest_ysi.R` written and verified (2026-10-28); `check_dat_contract.R` written with tests (2026-10-28); `reference/dat_contract.md` updated for enforcement, units and lookup rules (2026-10-28). **Open: wire the check into the combine step; decide the lookup-coverage warning (add YSI, TSS, FC methods and the `SW8260D` key to the three sheets, then promote to error); compare per-type `lab_qc` counts with `dat_contract.md`; test a Trip Blank label in `resolve_rm_site`** |
| C | `qaqc.qmd` Parts B-D; field blank coding; surrogate check in Q20; Q17 fixes; write `assessed_results.csv` and `lab_qc_assessed.csv` | Pending |
| D | `prelim_report.qmd` generalized from `other/documents/preliminary_report_2025/2025_preliminary_summary.qmd`; its setup chunk (about lines 200-530) re-ingests raw data and is replaced by reading `assessed_results.csv` | Pending |
| E | Blank the template (see Working Model), keep the trimmed example, update both repos' docs | Pending |

Key decisions are in the plan. The ones that most affect day-to-day work: `dat` has 24 columns (see `reference/dat_contract.md`); field blanks use Activity Type `Quality Control Sample-Field Blank` and Activity ID suffix `FB`; `lab_qc_dat` holds all 14 SGS lab QC codes plus any `(surr)` analyte and never flows to the WQX export; `prelim_report.qmd` reads only `output/assessed_results.csv`, `config.yml` and reference data.

## Current Layout

The pipeline is mid-migration. Today the working pipeline is `project_year/2025.qmd`; the target is `qaqc.qmd` plus `prelim_report.qmd`.

```
kenai-river-wqx-qaqc/
├── config.yml                  # Year settings (dates, lab file paths/sheets, FC and TSS run/receipt date+time, sgs trip_blank_sites, tss lod/loq)
├── data/                       # One year's raw lab files; intended to be committed per year repo (15 files, 31 MB for 2025; currently untracked, and nothing from the Phase B work is committed yet)
├── functions/                  # ingest_sgs.R, ingest_fc.R, ingest_tss.R, ingest_ysi.R, resolve_rm_site.R, clock_utils.R, check_dat_contract.R (Phase B);
│                               # format_wqx.R, apply_qaqc_flags.R, generate_cdx_export.R
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
├── _quarto.yml                 # type: book, output-dir output/report; chapters index, intro, references, plus project_year/2025.qmd
└── README.md                   # STALE: still describes archive/fetch; to be rewritten as a copy-the-template checklist
```

Governance PDFs (QAPP, CALM, 18 AAC 70, etc.) stay in the report repo's `other/agent_context/`; text versions are in `other/documents/md/` here. WQX Web import configuration (Uid 9481) walkthrough is in `cdx_upload.qmd` here (moved from the report repo 2026-10-28); upload and delete history is in `other/documents/cdx_upload_history/`. The older config export (Uid 8515, `KWF_Results_Baseline_Template`) is in that folder; the working config (Uid 9481) is `other/documents/EPA_WQX_templates/cdx_import_config_results_activities_20261028.cfg`.

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

## Per-Lab Ingest Functions (Phase B, replace the inline chunks below)

Each returns `list(dat, lab_qc)`, 24-column `dat`, no `matrix` or `result_detection_condition`, units in project spelling (`mg/L`, `ug/L`, `cfu/100ml`; see the units decision under `check_dat_contract.R`). `2025.qmd` still uses its own inline chunks and is unchanged.

- **`ingest_sgs.R`** `ingest_sgs(season, config, ...)`: routes the 14 lab QC types and any `(surr)` analyte to `lab_qc`; stops on NA location ID, NA `epa_analysis_id`, or non-WQX `mg/L` case; `expand_trip_blanks()` reads `config.yml` `sgs: trip_blank_sites`. 2025: `dat` 543 rows, `lab_qc` 914 rows.
- **`resolve_rm_site.R`** `resolve_rm_site(labels, sites_path)`: shared by FC, TSS, YSI. Parses the river-mile number from a lab label and matches `river_mile` in `reference/baseline_sites.csv`; strips any `KENAI_WQX-` prefix. Condition comes from label text, never site lists: `DUP` = Field Duplicate; `BLANK` + RM number = Field Blank (Trip Blank if `TRIP`); `BLANK` without RM = lab MB; `POSITIVE` = LCS. Handles `RM O`, `RM22`, `RM_10_DUP`. Stops listing unresolved labels. `&` multi-site trip blanks are handled by `ingest_sgs` via config. Trip Blank labels not yet tested.
- **`ingest_fc.R`** `ingest_fc(season, config, data_dir, sites_path)`: finds the header row by searching "Dish Number" (no `skip` setting); stops if the file's `DATE IN` differs from config sampling date; `parse_clock()` detects Excel-fraction vs HHMM per column. Fixed values: units `cfu/100ml`, method and `epa_analysis_id` `9222D`, context `APHA`, `loq` 1, lab `SWWTP`. `run_date` = `collect_date` (user-stated: FC always runs on collection day); `run_time` = `Time In`. `rec_date`/`rec_time` come from `config$files$fc[[season]]` (COC only; warns and sets NA if missing; **2025 left blank until the COC is available**). `N/A` results dropped with a message; zero count gives `result` NA and flag "U". BLANK rows become MB (`qc_group` blank), POSITIVE become LCS (accuracy); `lab_qc` has an extra `result_text` column (summer positive control reads `TNTC`); a non-numeric result in a field sample stops the run. Verified against old `dat_fc`: all 46 rows match. **To reconcile:** SGS `lab_qc` lacks `result_text`.
  - FC facts (2026-10-28): 15 zero counts became NA with "U"; `lab_qc` has 8 rows (3 blanks and 1 positive per season); spring RM 6.5 and RM 10.1 dropped as N/A. `note` holds analysis volume plus DATE OUT and Time Out. Unit evidence: WQP shows `cfu/100mL` for KWF FC 2000-2004 only; `2023.qmd` and the 2021 scripts use `cfu/100ml`.
  - `resolve_rm_site()` returns `label`, `monitoring_location_id`, `sample_condition`, `lab_qc_type`; `river_mile` is unique across the 22 sites (10 vs 10.1). Tested: `RM 22 DUP`, `RM22`, `RM_10_DUP`, `RM O`, `RM 10.1`, `BLANK`, `POSITIVE CONTROL`, `RM 18 FIELD BLANK`; `RM 99` stops.
  - `ingest_sgs` verification: `all.equal` TRUE on 24 columns versus old `dat_sgs` (location ID cast to character). `lab_qc` 914 rows: 279 calibration, 315 blank, 199 accuracy, 85 other, 36 surrogate (spring has none). Pending: compare per-type counts with `reference/dat_contract.md`.
  - 2025 test-fixture counts only (never a template expectation; row counts vary by year, so the contract-check function must validate structure and rules, not row counts): combined `dat` is 857 rows (SGS 543, FC 46, TSS 46, YSI 222), versus 883 in the legacy pipeline.

- **`clock_utils.R`**: `parse_clock()` (Excel fraction or HHMM, detected per column) and `parse_clock_text()` ("13:31"). Sourced by `ingest_fc.R`, `ingest_tss.R` and `ingest_ysi.R`.
- **`ingest_ysi.R`** `ingest_ysi(config, data_dir, site_names_path, corrections_path)`: no season argument; one file for both seasons (`files$ysi$both$path`). Returns `list(dat, lab_qc, excluded)` (`lab_qc` is an empty tibble). Decisions (2026-10-28):
  - `collect_date` = `Site Depart Date`; stops if it matches neither `sampling$spring_date` nor `summer_date`. Template rows (no site, parameter or value) dropped; site visits with no values dropped with a message (2025: spring Cunningham Park and RM 10.1).
  - Sites resolve through `reference/ysi_site_names.csv` (37 spellings; moved out of `2025.qmd`); unmapped names stop the run.
  - Implausible values (pH outside `ysi: {ph_min, ph_max}` in `config.yml`, DO above `ysi: do_max`) are removed from `dat` and returned in `excluded` for the QA/QC document (Phase C must report it) so the user can check and correct manually. Corrections go in `year_files/ysi_corrections.csv` (site_name, collect_date, parameter, replicate, corrected_value or `keep`, reason), applied before the check and averaging; a row that does not match exactly one data row stops the run. 2025 `excluded`: 8 replicates (spring DO 167 at Soldotna Creek and 22 to 33 mg/L at RM 74, RM 70 and RM 79.5; pH -2.3 at RM 82); the file is header-only until reviewed.
  - `collect_time`: `Collection Time` exists only on the turbidity rows, so the visit's Collection Time is used for all five parameters (user decision; `Observation Time` is ignored, it runs a median 51 minutes later). The old chunk would have given NA for the four meter parameters.
  - `lab_name` = `Meter Model` from the file (YSI ProQuatro or Hach for turbidity), filled within site, date and parameter. Units `deg C`, `uS/cm`, `mg/L`, `NTU`, `None`; stops if the file's unit contradicts. `rec_date`/`rec_time` NA; `run_date`/`run_time` = collection. Replicates averaged per site, date, parameter (spring Moose River has a third); `note` is "Mean of N replicate(s)" or "Mean of N of M replicate(s)" when some were excluded (2025: Soldotna Creek DO and RM 82 pH).
  - Verified: 222 rows (102 spring, 120 summer), 22 sites, 24 columns, all 222 means match the old chunk's logic (max difference 3e-7), and the 8 excluded replicates are the old filter's drops.

- **`check_dat_contract.R`** `check_dat_contract(dat, config, lab_qc = NULL, ref_dir, stop_on_error = TRUE)`: validates structure and rules only, never counts. Returns an issues tibble (check, severity, n_rows, labs, examples) and stops on errors. Checks: 24 columns with exact names and types, no extras; no NA in required columns; flag values; `result` NA only with U or ND (a number with U is allowed, SGS reports the detection limit that way); units in the WQX UoM list or `project_unit_spellings.csv` and no capitalization clashes; bare-digit location IDs present in `site_coordinates`; no lab QC `sample_type` or `(surr)` analyte in `dat`; `sample_condition` values; analyte in `analytes_list_manual_edit.csv`; unique result key; `collect_date` in the two config sampling dates and in `config$year`; `run_date` and `rec_date` order. Minimal `lab_qc` check (required columns, `qc_group` values, units). Tests: `tests/test_check_dat_contract.R` (synthetic data; 10 tests, 30 expectations, all passing; needs `testthat`). On the 2025 fixture: no errors, 3 warnings. Lookup misses are warnings because `format_wqx.R` left-joins and does not stop on a miss.
  - **Lookup coverage (warning, not error):** `analytical_method` is missing from the `result_sample_fraction`, `chemical_preservative` and `sample_container_type_color` sheets for YSI, TSS and FC methods, and the sheets hold `8260D` while dat carries `SW8260D` (so BTEX likely gets NA fraction, preservative and container in `format_wqx.R`). Needs a decision: add rows (and the SW8260D key), then promote to error.
  - **Units decision (2026-10-28):** the WQX UoM list (`Results` template's `UoM` tab, snapshot in `reference/wqx_templates/wqx_uom_codes.csv`, 364 codes) spells `mg/l`, `ug/l`, `cfu/100ml`. WQP data (2,683 KWF metals rows) and the accepted 2021 upload use `ug/L` and `mg/L`, so the project keeps `mg/L` and `ug/L` (`project_unit_spellings.csv`), checked case-insensitively against the list. FC units changed to `cfu/100ml` (matches the list, WQP and 2021); this supersedes the earlier `cfu/100mL` rule. Whether WQX Web normalizes case on import is unverified (a trial import would settle it). The `unedited_templates` folder already held identical copies of the WQX workbooks, so nothing needed copying.

Settled rules for the remaining functions:
- FC units are `cfu/100ml` (changed 2026-10-28 from `cfu/100mL`: matches the WQX UoM list, the 2021 upload and `2023.qmd`).
- YSI `rec_date`/`rec_time` must be NA (in situ, no lab); the old chunk wrongly copies collect time.
- Q17 (Phase C): missing receipt is "not evaluable" (old code turns it into midnight); YSI is "not applicable"; a method with no holding time must not default to "Y". `sample_holding_times.csv` has `9222D` at 6 hours (collection to analysis start, i.e. `Time In`).
- Per-year edits for the README checklist: new rows in `sgs_site_names_matching_table.xlsx` for new SGS duplicate/blank IDs; `sgs: trip_blank_sites`; FC receipt date and time from the COC per season.

- **`ingest_tss.R`** `ingest_tss(season, config, data_dir, sites_path)`: returns `list(dat, lab_qc)`; reuses `parse_clock()`/`parse_clock_text()` from `clock_utils.R` and `resolve_rm_site()`. Finds the header row by searching `Sample_Location`; `sample_type` must be `PS` (field) or `MB` (method blank), else stops. Decisions (2026-10-28): LOD/LOQ come from `config.yml` `tss: {lod, loq}` (0.31 / 1.0 mg/L, QAPP v3 Table 5 for SM21-2540-+D; check against the SWWTP report each year; stops if missing or lod >= loq). `run_date` and `run_time` are **required** in `config.yml` `files$tss$<season>` (copy "DATE OF ANALYSIS" and "TIME" from the lab sheet header); summer files carry `Date_of_Analysis` per row and the function stops if it disagrees with config. `rec_date`/`rec_time` optional (from the COC; warns and sets NA if blank; 2025 left blank). `collect_date` from `sampling$<season>_date` (not the file). Flags: below LOD gives `result` NA and "U"; between LOD and LOQ gives "J"; else "=". Sites with no weights and no sample time are dropped with a message (not sampled); a weight without a time (or reverse) stops. MB rows go to `lab_qc` (qc_group `blank`); `dat` carries units `mg/L`, method and `epa_analysis_id` `2540-D`, context `APHA`, lab `SWWTP`, note "Sample volume = N mL". Fixes the `mg/l` units contract violation. Reference: `other/misc/report_repo_scripts/ingest_tss.R`.
  - Verified 2026-10-28 against old `dat_tss`: new `dat` has 46 rows (22 spring, 24 summer) versus 48 old; all 46 match on site, date, condition, result, flag and collect time. The 2 old-only rows are spring RM 6.5 and RM 10.1, not visited (user-confirmed). LOD 0.5 to 0.31 changed no flags. `lab_qc`: 4 MB rows per season (spring 0.333, 0, 0.333, 0 = two J, two U; summer all 0 = U); blank results kept as reported. Receipt warning fires for both seasons (expected until the COCs are entered).
  - Other behavior: results rounded to 6 decimals (spreadsheet noise); sources `clock_utils.R` if `parse_clock()` is not loaded; stops if a PS label resolves to a lab QC type; analyte "Total suspended solids"; `result_sample_fraction` left NA (`format_wqx.R` sets "Suspended"); `lab_qc` has no `result_text`; `repdl` = LOQ.
  - Sampling facts: the spring tidy sheet's `Field_Sample_Date` (5/1) is the analysis date under a wrong heading, so `collect_date` always comes from config (spring 4/30, summer 7/23). Spring `run_time` 13:58 comes from the original `.xls` header; summer is 1400 on 07-25-25. Collection and run dates differ (1 to 2 days); TSS holding time (`2540-D`) is 7 days, measured collection to analysis start, so a blank receipt does not block Q17 for TSS.
  - Method ID `2540-D` (context `APHA`) matches the 2021 example output and the 2025 export; `SM21-2540-+D` appears only in the QAPP, the report repo script and the `config.yml` `tss:` comment. 2025 TSS files contain no lab duplicates or check standards.

## 2025.qmd Pipeline Structure (legacy inline version, being replaced)

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

## Book and Docs (2026-10-28)

`quarto render` ran clean. `intro.qmd` was rewritten (purpose, single-year workflow, four pipeline parts, bare-ID rule). `summary.qmd` is a seven-step finish-a-year checklist (not a book chapter; can feed the README checklist). Book title no longer says "2014 - 2025"; `date: today` is set in `_quarto.yml`. Output stays `output/report`; whether it is published (GitHub Pages) is open. Not checked: whether Part D wrote files or skipped.

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

- **Units case:** 48 SWWTP TSS rows carry `mg/l` in `2025.qmd`, which hardcodes it. Fixed in `functions/ingest_tss.R`; `2025.qmd` itself still has the problem.
- **Surrogates leaking:** 24 BTEX surrogate recovery rows (units `%`) are in `dat` and in the 2025 WQX export as `Field Msr/Obs`. They belong in `lab_qc_dat`.
- **Dead branch:** `format_wqx.R` tests `sample_condition == "Blank"`, but ingest emits `Trip Blank` and `Field Blank`.
- **Field blanks not coded:** 44 `Field Blank` rows in `dat`; the export has no field-blank Activity Type, and 56 Activity IDs end in `-NA`.
- **Duplicate Activity IDs:** 116 rows in the 2025 output share an Activity ID with the same characteristic, fraction and method. Not caused by the ID change; investigate before upload.
- **Duplicated spring and summer ingest chunks** (resolved for SGS, FC, TSS and YSI by the new functions).
- **YSI receipt:** `2025.qmd` copies collect time into `rec_date`/`rec_time`; should be NA (fixed in `ingest_ysi.R`).
- **Citations:** `index.qmd` cites `@mccard2007` and `@guerronorejuela2016`, missing from `references.bib`.
- **Stale docs:** `README.md`, the plan file (archive/fetch content, item 13) need updating (`reference/dat_contract.md` was refreshed 2026-10-28; recheck it once Phase C settles). Defer until code settles. Small items: the `config.yml` `tss:` comment cites `SM21-2540-+D` while the function emits `2540-D`; the TSS/FC comment lines in `config.yml` sit above the `fc:` block. `2025.qmd` still hardcodes TSS `mg/l` and the 0.5 LOD.

Other:

- **Function copies differ** from the report repo's `functions/appendix_a_scripts/` (large diffs in `format_wqx.R` and `generate_cdx_export.R`). Reconcile in Phase B; the report repo's `generate_cdx_export.R` has the bare-ID guard and the `na = ""` fix.
- **WQP downloads:** `other/input/WQX_downloads/` must be populated manually before Part D runs; Part D prints a message and skips if absent.
- **2021 on WQP:** corrected 2021 files were re-uploaded 2026-10-25 (CDX datasets 117639, 117640, 117643, all Completed at CDX). A live query on 2026-10-28 showed 0 rows for 2021 and no doubled-prefix stations; recheck about Nov 1-3. Tracked in the report repo.
- **TSS LOD/LOQ:** now 0.31 / 1.0 mg/L in `config.yml` (`tss:` block); `2025.qmd` still uses the 0.5 / 1.0 placeholders. Confirm against the SWWTP lab report.
- **FC run_date:** resolved. `ingest_fc` sets `run_date` = `collect_date`; the lab file headers show DATE OUT one day later (spring 05-01-25, summer 07-24-25), recorded in `note` with Time Out. Do not carry the 2021 receipt times (13:31, 13:37) from the report repo into any year.
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

2021 WQP verification, cache refresh and book render; trend analysis rollout; characteristic name audit (Task 1b); CALM 5-year sample counts (Task 1c); `review_needed` verification in `master_reg_limits.xlsx` (Task 5). See that repo's `AGENTS.md`.

## Style Preferences
- This repo is a blank template for any year. Never record or code row counts, site counts or other 2025 values as expectations; label 2025 numbers as test-fixture results. Checks must test structure and rules (columns, types, units, allowed values, config consistency).

- Do not put conversation-title markup literally in `/compact` summaries (it truncated a summary). Approval of a design is the go-ahead to write it.
- No em-dashes; use a colon, comma, semicolon or parentheses. Minimal AI-writing tropes.
- All ingest scripts must emit `mg/L` (WQX capitalization).
- Do not interpret trends in text.
- Before implementing code, explain what it does and why, and wait for confirmation. Present decisions one at a time, not as a batch.
