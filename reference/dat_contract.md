# `dat` Column Contract (DRAFT, Phase A step 1)

Status: draft derived from `project_year/2025.qmd` Part A, Part B (`format_wqx.R`), and
a profile of the actual 2025 `dat` (883 rows, 22 sites, 27 analytes, three labs).
Nothing here is enforced in code yet. A contract-check function will be added in Phase B.

Reference year: the 2021 pipeline in the report repo (`chapters/appendix_a.qmd` and
`functions/appendix_a_scripts/`) is the most thoroughly troubleshot year. Where 2021 and
2025 disagree, 2021 behavior is the default unless there is a stated reason to differ.
Other years received little troubleshooting, so their partial work is reused only where
it helps.

## Purpose

`dat` is the seam between ingestion and everything else. Each per-lab ingest function
(`ingest_sgs`, `ingest_fc`, `ingest_tss`, `ingest_ysi`) returns a data frame that
satisfies this contract. The results are row-bound into one `dat`. Everything after that
(WQX formatting, QA/QC flags, CDX export, preliminary report) assumes the contract and
never touches raw lab files. Year-specific work (file names, sheet names, site-name
spellings, impossible-value exclusions) ends at this boundary.

One row is one reported result for one sample, analyte, and fraction. Lab QC rows
(method blanks, LCS, MS/MSD, lab duplicates, and BTEX surrogate recoveries) are NOT in
`dat`. They are ingested in the same step, designated as lab QC samples, and written to a
second table, `lab_qc_dat` (in 2021: `sgs_als_qaqc_dat.csv`), which feeds checklist
Question 20 (lab discrepancies, matrix spikes, blanks, QC failures). `lab_qc_dat` keeps
the extra raw columns that field data does not need (`percent_recovered`,
`rec_limit_low`, `rec_limit_high`, `sample_rpd`, `rpd_limit_low`, `rpd_limit_high`,
`dissolved`, etc.). Its contract is still to be written.

## Columns (24)

"Req" means required by `format_wqx.R` or later steps, so the column must exist.
"NA ok" means the value may be missing for some rows.

| # | Column | Type | Req | NA ok | Meaning and rules |
|---|---|---|---|---|---|
| 1 | `sample` | chr | yes | no | Lab/field sample label as it appears in the source file. Used for duplicate pairing and trip-blank handling. |
| 2 | `lab_sample` | chr | yes | yes | Lab's own sample ID (`lab_sample_id`). NA for YSI; NA for FC in 2025. Maps to WQX Laboratory Sample ID. |
| 3 | `lab_name` | chr | yes | no | One of `SGS`, `SWWTP`, `YSI` (2025). Maps to WQX Laboratory Name. New labs add a value here. |
| 4 | `analyte` | chr | yes | no | Name as reported or mapped at ingest. Must match a row in `analytes_list_manual_edit.csv` (abbreviation for Activity ID) and later the WQX characteristic list. |
| 5 | `result` | dbl | yes | see rule | Numeric value. NA is allowed only for a non-detect where the lab reported no number (`resultflag == "U"`); 2025 has 3 such TSS rows. |
| 6 | `units` | chr | yes | no | WQX capitalization, e.g. `mg/L`, `ug/L`, `uS/cm`, `deg C`, `NTU`, `MPN/100ml`, `None`. See "Known contract violations". |
| 7 | `resultflag` | chr | yes | no | `=` (detected), `U` (not detected), `J` (estimated, below LOQ). `format_wqx.R` also accepts `ND` as not detected. |
| 8 | `loq` | dbl | yes | yes | Limit of quantitation, in `units`. NA for YSI and some FC/TSS. |
| 9 | `lod` | dbl | yes | yes | Limit of detection / method detection level, in `units`. NA for YSI. |
| 10 | `collect_date` | Date | yes | no | Sample collection date. |
| 11 | `collect_time` | hms | yes | yes | Collection time. Unrecorded for most YSI rows in 2025 (177 of 222). |
| 12 | `rec_date` | Date | no | yes | Lab receipt date. Used in the hold-time check (Q17). YSI sets it equal to `collect_date`. |
| 13 | `rec_time` | hms | no | yes | Lab receipt time. |
| 14 | `run_date` | Date | yes | no | Analysis date. Maps to WQX Analysis Start Date. |
| 15 | `run_time` | hms | yes | yes | Analysis time. |
| 16 | `analytical_method` | chr | yes | no | Method as the lab reports it, with any `EP` prefix removed (`200.8`, `SM21 4500NO3-F`, `2540-D`, `9222D`, `SW8260D`). `format_wqx.R` joins fraction, preservative, and container lookups on this value. |
| 17 | `epa_analysis_id` | chr | yes | no | Method ID for WQX Result Analytical Method ID. Equals `analytical_method` for YSI; differs from it in 164 of 883 rows via the analysis-code matching table. |
| 18 | `context_code` | chr | yes | no | WQX method context, e.g. `USEPA`, `APHA`. |
| 19 | `monitoring_location_id` | chr | yes | no | Bare numeric ID, e.g. `10000002`, with NO `KENAI_WQX-` prefix (WQX Web adds it; a prefix produced the doubled-station problem in October 2026). Stored as character. Must exist in `site_coordinates`. |
| 20 | `note` | chr | no | yes | Free text; becomes Activity Comment. SGS sets it to the SDG number; other labs NA. |
| 21 | `sample_type` | chr | yes | no | Lab sample-type code. In `dat`: `PS` (primary sample) or `TB` (trip blank) in 2025. Lab QC codes (`MB`, `LCS`, ...) must have been removed. |
| 22 | `sample_condition` | chr | yes | yes | NA for primary samples, otherwise `Field Duplicate`, `Field Blank`, or `Trip Blank`. Drives Activity ID suffix and Activity Type. |
| 23 | `dissolved` | chr | no | yes | SGS's raw flag (`L` filtered, `.` unfiltered, `T`). NA for non-SGS rows. Kept in `dat` (user decision 2026-10-28) as well as in `lab_qc_dat`; `result_sample_fraction` is the working field. |
| 24 | `result_sample_fraction` | chr | yes | yes | `Dissolved`, `Total`, `Unfiltered`, or NA. NA is acceptable at ingest; `format_wqx.R` fills it from `wqx_template_matching_table.xlsx` and overrides TSS (`Suspended`) and FC (`None`). A value set at ingest wins over the lookup. |

## Invariants the contract check should test

1. All 24 columns present with the types above (`Date`, `hms`, `dbl`, `chr`).
2. `lab_name`, `analyte`, `units`, `resultflag`, `analytical_method`, `epa_analysis_id`, `context_code`, `monitoring_location_id`, `sample_type`, `collect_date`, `run_date`, `sample` have no NA.
3. `resultflag %in% c("=", "U", "J", "ND")`. `result` is non-NA unless `resultflag` is `U` or `ND`.
4. `units` uses WQX capitalization; no lowercase `mg/l`.
5. `monitoring_location_id` matches `^[0-9]+$` (no prefix) and is present in `site_coordinates`.
6. No lab QC sample types (`MB`, `LCS`, `LCSD`, `MS`, `MSD`, `CB`, `OS`, `ICV`, `CCV`, `LLQC`, `QCS`, `ICB`, `CVC`, `IB`).
7. `sample_condition` is NA or in `{Field Duplicate, Field Blank, Trip Blank}`.
8. `analyte` is in the analyte abbreviation list; `analytical_method` is in the fraction, preservative, and container lookups (unmapped values should stop the run, not silently produce NA).
9. No duplicated key of (`monitoring_location_id`, `collect_date`, `analyte`, `result_sample_fraction`, `sample_condition`, `epa_analysis_id`). Activity ID uniqueness is what the WQX import depends on.

## Known contract violations in the current 2025 `dat`

Found by running Part A of `2025.qmd` in a separate R process and profiling the result.

1. **`units` mixes `mg/l` and `mg/L`.** All 48 `mg/l` rows are SWWTP total suspended solids. Part A of `2025.qmd` writes `"mg/l"` literally in both the spring and summer TSS chunks. The report repo fixed the same bug in its own `ingest_tss.R` on 2026-10-20; the qaqc repo copy still has it. Whether `format_wqx.R` or the export fixes it later is unverified.
2. **`matrix` is `"1"` for all 567 SGS rows** and `"Water"` for FC, TSS, and YSI. The SGS EDD column carries a numeric code. `format_wqx.R` does not read `matrix`, so nothing breaks, so the column is dropped from `dat` (resolved 2026-10-28).
3. **BTEX surrogate recoveries leaked into `dat` and into the WQX file.** Three analytes (`1,2-Dichloroethane-D4 (surr)`, `4-Bromofluorobenzene (surr)`, `Toluene-d8 (surr)`; units `%`; 24 rows) pass through Part A of `2025.qmd` because its `lab_qc_types` list has no surrogate code, and all 24 appear in `2025_kwf_baseline_results_wqx.csv` as `Field Msr/Obs`. The 2021 pipeline handled these correctly: `ingest_sgs_als.R` routes any analyte containing `(surr)` to `sgs_als_qaqc_dat.csv` and removes it from the field data. **Decision (user, 2026-10-28):** keep surrogates in the ingest as designated lab QC samples, in `lab_qc_dat`, not in `dat`. The template should also evaluate them under Question 20 (the 2021 chapter reviews matrix spikes only).
4. **`result_detection_condition` is set at ingest and then overwritten** by `format_wqx.R`. Dropped from `dat` (resolved 2026-10-28).
5. **Spring and summer ingest code is duplicated** (SGS, FC, TSS each have two near-identical chunks), which is part of why per-lab functions taking a season argument are the Phase B target.

## Open questions

- RESOLVED 2026-10-28: `matrix` and `result_detection_condition` are dropped from `dat` (constant and recomputed by `format_wqx.R`, respectively). `dissolved` stays in both `dat` and `lab_qc_dat`.
- RESOLVED 2026-10-28: field blank coding, see "Field blank handling" below.
- Is `sample_type` needed beyond `PS`/`TB`? Field duplicates are already carried by `sample_condition`; `format_wqx.R` also tests `sample_condition == "Blank"`, which no ingest step produces (it emits `Trip Blank` and `Field Blank`), so that branch looks dead and its effect on Activity Type for blanks should be confirmed.
- Should the assessed output (`assessed_results.csv`, Phase C) use this schema plus flag columns, or the WQX-formatted names? Not decided; to be settled when Phase C is scoped.

## Field blank handling (decided 2026-10-28)

`Field Blank` is a `sample_condition` value in 2025 (44 `dat` rows; the 2022 QAPP change introduced a dissolved metals field blank). Years before about late 2022 have none, so field-blank handling is optional per year.

**Decision (user, 2026-10-28):**
- **Activity Type:** `Quality Control Sample-Field Blank` (QFB), the code in the WQX ActivityType domain list. It requires an analytical method; no monitoring location constraint.
- **Activity ID suffix:** `FB`. The existing trip blank suffix `Blank` is unchanged, so Activity IDs already on WQP do not change. A dissolved and a total field blank for the same analyte, site, and date share one Activity ID; `Result Sample Fraction` distinguishes them.
- **How's My Waterway:** HMW's measurement charts drop any record whose Activity Type contains "quality" (`MonitoringReport.tsx`), so this code keeps field blanks out of HMW charts. Station summary counts and WQP downloads are not filtered.

**Current defect in `format_wqx.R` (2025):** the suffix and Activity Type logic knows only `Field Duplicate` and `Trip Blank`. In the 2025 intermediate export, 56 Activity IDs end in `-NA` (for example `10000002-2025-04-30-Cd-NA`), consistent with field blanks getting no suffix (44 field blanks; the extra 12 rows are not yet explained). All 883 rows are `Field Msr/Obs` (767), field replicate (80), or trip blank (36), so field blanks would upload as ordinary field results. Fix in the template before any 2025 upload, and add a contract invariant: every `Field Blank` row maps to the QFB Activity Type and an `FB` suffix.

## `lab_qc_dat` (profiled from 2025 SGS EDDs, 2026-10-28)

Both 2025 SGS files (Sheet8) share the same 42 columns. `sample_type` values: PS (primary sample), TB (trip blank, summer only, 18 rows; field data, not lab QC), and 14 lab QC codes. Counts are spring/summer combined.

| Group | Codes (rows) | Limits populated |
|---|---|---|
| Accuracy (spike recovery) | LCS (72), LCSD (15), MS (65), MSD (14), QCS (39) | `percent_recovered`, `rec_limit_low/high` always; `sample_rpd`, `rpd_limit_low/high` for LCSD (RPD limits 80%) and MSD |
| Calibration checks | ICV (39), CCV (237), CVC (9) | recovery and limits always |
| Blanks | MB (72), CB (227), ICB (13), IB (9) | no recovery or limits; result vs `repdl`/LOQ is the check (a few MB/IB rows carry recovery, apparently surrogates) |
| Other | LLQC (45), OS (40) | LLQC: recovery but no limits; OS: no recovery or limits |

Findings:
- **Limits ship with the EDD.** Recovery and RPD limits are columns, so Question 20 can use the lab's own limits and no hardcoded thresholds are needed (except LLQC and OS, which have none).
- **Surrogates (`(surr)` analytes, 24 rows in PS, plus rows within QC codes such as CCV, CVC, IB, LCS)** appear in both field samples and QC samples. Rule: route any `(surr)` analyte to `lab_qc_dat` regardless of `sample_type`.
- **`lab_qc_types` in `2025.qmd` is the right list** (MB, LCS, LCSD, MS, MSD, CB, OS, ICV, CCV, LLQC, QCS, ICB, CVC, IB), but it omits the surrogate rule. TB is correctly kept out.
- **Proposed `lab_qc_dat` columns:** `sample_type`, `qc_group` (accuracy, calibration, blank, other; derived), `analyte`, `analytical_method`, `result`, `resultflag`, `repdl`, `units`, `amount_spiked`, `initial_conc`, `percent_recovered`, `rec_limit_low/high`, `sample_rpd`, `rpd_limit_low/high`, `anl_batch`, `prp_batch`, `sdg`, `lab_sample_id`, `sample_id`, `collect_date`, `run_date`, `dissolved`, `lab_name`, `season`. Spring/summer file differences: none found.
- **Not yet checked:** whether the 2021 ALS layout carries the same limit columns; this is the first adapter test.
