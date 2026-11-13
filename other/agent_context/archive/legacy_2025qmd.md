# Legacy 2025.qmd pipeline and 2023.qmd status

Moved verbatim from AGENTS.md on 2026-11-13. Read when relevant; core memory is AGENTS.md.

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

## 2023.qmd Status

Rebuilt to match 2025 template structure (YAML at top, `date: today`, Part A/B/C/D layout, `here::here()` paths). All 22 code chunks are `eval: false`, pipeline not yet ported to new template; document renders as book chapter without executing code.

**2023-specific data notes (preserved in Part A notes block):** - Spring sampling: 2023-05-02. Summer: 2023-07-18. - SGS spring: two CSV files (Revision 1, part1 + part2). Summer: XLSX Sheet9. - Summer 2023: EPA 200.8 used for both total AND dissolved metals (no 200.7). `ANALYSIS_GROUP` column distinguishes total vs. dissolved. - Spring receipt date corrected: EDD shows 05/09/2023; actual receipt was 05/04/2023 08:51. - Spring collect date corrected: some samples show 2023-05-03 (COC typo) → 2023-05-02. - SW846 6010D rows filtered out (no reportable values). - Spring 2023: RM 10.1 not visited (low water); RM 40 FC sample spilled on-site. - Dissolved Cu and Zn flagged: dissolved \> total in 32 of 52 pairs; two spring dissolved Zn method blanks above LOQ. - TSS LOD = 0.31 mg/L. Spring receipt times from COC ReadMe: RM_1.5 = 15:30, others = 12:44; summer = 12:25.

------------------------------------------------------------------------

