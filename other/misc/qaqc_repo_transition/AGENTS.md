# Kenai River WQX — Annual QA/QC Pipeline Repo: Posit Assistant Context

## Companion Files (load on demand, not every session)

- `reference/2021_ingest_sgs_als.R` — 2021 worked example: SGS/ALS ingest
- `reference/2021_ingest_fc.R` — 2021 worked example: Fecal Coliform ingest
- `reference/2021_ingest_tss.R` — 2021 worked example: TSS ingest
- Report repo session log (if needed): see `other/agent_context/session_log.md` in the report repo

---

## Next Session Priorities

**EPA WQX sync issue BLOCKED — do not attempt CDX delete or re-upload until EPA confirms ETL
is restored (~April 23).**

1. **Task 19 (HIGH)** — Build `2023.qmd` from `templates/pipeline_template.qmd`. Adapt Part A
   for 2023 EDD format quirks. See 2023-specific notes below.
2. **Task 1b (HIGH)** — Audit all distinct `CharacteristicName` values in WQP for org
   `KENAI_WQX`. Cross-reference against current WQX domain list. Map variants to canonical names.
3. **Task 1c (HIGH)** — CALM 5-year window sample count check (2017-2021). Count
   `result_status_identifier == "Accepted"` results per parameter + site for
   `activity_start_date >= 2017-01-01`. Source: 2021 flagged export CSV. Flag combinations
   below 10 (or 5 for toxics).
4. **Task 2 (Medium)** — Fix How's My Waterway visibility for 15 legacy numeric-ID stations.
   Process lives here in the qaqc repo.

---

## Active Tasks

| # | Priority | Description | Status |
|---|----------|-------------|--------|
| 1a-reupload | HIGH, BLOCKED | Re-upload 835 2021 records. Files ready in report repo. Waiting for EPA ETL fix. | Blocked ~April 23 |
| 1b | HIGH | Characteristic name audit across all KWF years in WQP | Pending |
| 1c | HIGH | CALM 5-year window sample count check (2017-2021) | Pending |
| 2 | Medium | Fix HMW visibility for 15 legacy numeric-ID stations | Pending |
| 5 | Medium | Verify 6 `review_needed = Y` rows in `standard_types` sheet of `master_reg_limits.xlsx` | Pending |
| 19 | HIGH | Build `2023.qmd` from template; adapt Part A for 2023 EDD quirks | Pending |
| 7 | Low | Resolve ALS lab duplicate (DUP) status issue (4 results) | Pending |
| 12 | Low | Spring 2013 specific conductance: stored as mS/cm, should be uS/cm | Pending |

---

## Style Preferences

- **No em-dashes.** Replace with colon, comma, semicolon, or parentheses. Applies to all
  `.qmd` files, comments, and generated text. Note: pandoc converts `---` to an em-dash;
  use different punctuation instead.
- Use base R pipe `|>`. Do not use `%>%` in new code. Do not mass-convert legacy `%>%` usage.

---

## Collaboration Standard

Before implementing any code, explain what it does and why. Wait for user confirmation.
Prioritize clarity over cleverness.

---

## Pipeline Architecture

Each data year's complete QA/QC workflow lives in a single QMD: `{year}.qmd`.
The canonical template is `templates/pipeline_template.qmd`.

**Template structure:**

```
## Year Configuration        — sampling dates, file paths; ONLY block that changes every year
## Part A: Data Ingestion    — inlined code, adapted per year for EDD format quirks
   ### SGS/ALS Lab Results
   ### Fecal Coliform
   ### Total Suspended Solids
   ### Bind and Standardize  — produces standardized `dat` (the contract between A and B)
## Part B: WQX Formatting    — sourced: functions/format_wqx.R (stable)
## Part C: QA/QC Checklist   — questions 1-42; some formulaic, some manual entries
## Part D: Flag + CDX Export — sourced: functions/apply_qaqc_flags.R, generate_cdx_export.R
```

**Key design decisions:**
- Part A is **inlined** in the QMD so ingest code is visibly marked for adaptation.
- Parts B and D are **sourced scripts** because they are stable and should not change year to year.
- Part C is inlined; formulaic checks run from `cfg` paths; manual questions have `# MANUAL:` placeholders.
- The `reference/` scripts are for copy-and-adapt only, not for sourcing.

---

## cfg List Contract

All stable scripts (`format_wqx.R`, `apply_qaqc_flags.R`, `generate_cdx_export.R`) read paths
from a `cfg` list constructed in the Year Configuration block. Required keys:

| Key | Description |
|-----|-------------|
| `cfg$year` | Integer data year (e.g., 2023) |
| `cfg$templates_dir` | Path to `other/input/wqx_templates/` |
| `cfg$wqx_template_file` | Path to `AWQMS_KWF_Baseline_{year}.xlsx` |
| `cfg$spring_data_dir` | Path to spring raw lab data folder |
| `cfg$summer_data_dir` | Path to summer raw lab data folder |
| `cfg$output_qaqc_dir` | Path to `other/output/lab_qaqc_data/{year}_lab_qaqc_data/` |
| `cfg$wqx_intermediate_path` | Path to provisional WQX CSV (before flagging) |
| `cfg$flagged_export_path` | Path to flagged export CSV |
| `cfg$flag_decisions_path` | Path to `{year}_data_flag_decisions.csv` |
| `cfg$wqx_downloads_dir` | Path to `other/input/WQX_downloads/` |
| `cfg$spring_sample_date` | Spring sampling date as character "M/D/YYYY" |
| `cfg$summer_sample_date` | Summer sampling date as character "M/D/YYYY" |

Scalar date variables (used by ingest scripts) are also set as top-level globals in the
Year Configuration block: `year`, `spring_sample_date`, `summer_sample_date`, `spring_rec_date`,
`summer_rec_date`, `spring_fc_analysis_date`, `summer_fc_analysis_date`.

---

## `dat` Column Contract

After Part A completes and before `source("functions/format_wqx.R")` runs, `dat` must contain
at minimum these columns (exact names):

| Column | Type | Notes |
|--------|------|-------|
| `monitoring_location_id` | character | WQX site ID (e.g., "KR-RM-0-NNC") |
| `collect_date` | Date | |
| `collect_time` | hms | Can be NA for FC/TSS |
| `analyte` | character | CharacteristicName as it will appear in WQX |
| `result` | numeric | Reported value |
| `units` | character | e.g., "mg/L", "ug/L", "CFU/100mL" |
| `resultflag` | character | "=", "U", "J", or "ND" |
| `loq` | numeric | Limit of quantitation (can be NA) |
| `lod` | numeric | Method detection level (can be NA) |
| `analytical_method` | character | Full method string from lab EDD |
| `epa_analysis_id` | character | EPA method ID (e.g., "200.8") |
| `context_code` | character | Method context (e.g., "USEPA") |
| `lab_name` | character | Full lab name string |
| `lab_sample` | character | Lab sample ID |
| `run_date` | Date | Lab analysis date |
| `run_time` | hms | Lab analysis time (can be NA) |
| `note` | character | Activity comment (can be NA) |
| `sample_type` | character | "PS", "MB", "LCS", "TB", "DUP" |
| `sample_condition` | character | "Field Duplicate", "Trip Blank", NA for primary |

`format_wqx.R` will fail silently or produce incorrect output if any of these are missing or
misnamed. After the bind-and-standardize step, verify with `names(dat)` and `nrow(dat)`.

---

## FC Lab History

| Season | Lab | Notes |
|--------|-----|-------|
| Spring (all years) | SWWTP | |
| Summer 2022+ | SWWTP | Taurianen closed after 2021 |
| Summer 2021 | Taurianen | |
| Summer (some prior years) | Taurianen | Confirm exact years from KWF field records |

---

## 2023-Specific Notes (for Task 19)

- 2022 and later: Taurianen Engineering closed. Both spring and summer FC come from SWWTP.
  Set `spring_fc_lab <- "SWWTP"` and `summer_fc_lab <- "SWWTP"`.
- 2023 ALS lab supplied dissolved Ca/Fe/Mg. Verify units carefully (see 2021 unit correction
  note in `reference/2021_ingest_sgs_als.R` -- the same error may or may not recur).
- 2023 EDD column names from SGS may differ from 2021. Check with `names(spring_sgs_raw)`
  before relying on the 2021 column rename map.
- The Phosphorus 200.8 exclusion in `generate_cdx_export.R` was valid for 2021 (method not
  in QAPP). Confirm whether 2023 QAPP covers this method before applying or removing the filter.
- For years prior to 2022 (not just 2021): Taurianen Engineering was used for summer FC in
  multiple years before closing. Use the Taurianen parser block in `reference/2021_ingest_fc.R`
  for those years. Confirm which years from KWF field records.

---

## Sample Fraction Canonical Scheme

| Parameter type | Canonical fraction | Notes |
|---|---|---|
| Dissolved metals (any method, any filtration) | `Dissolved` | |
| Total metals (unfiltered) | `Unfiltered` | For 2023+: method alone no longer distinguishes |
| Nutrients | `Total` | |
| TSS | `Suspended` | |
| BTEX / volatiles | `Volatile` | |
| Fecal Coliform | `None` | |

---

## EPA WQX Flagging Convention (complete -- do not change)

- `Result Qualifier`: lab qualifiers from EDD (`U` = non-detect, `J` = below LOQ, `=` = detected)
- `Result Status ID`: KWF QA/QC decision -- `Accepted` or `Rejected`
- The binary `flag` (Y/N) in `{year}_data_flag_decisions.csv` maps to `Result Status ID` at CDX export
- Do not add FQC or other custom codes

---

## Trip Blank Crew Assignments

Trip blank-to-crew associations are year-specific and stored in per-year CSVs:
- Location: `other/input/wqx_templates/trip_blank_crews_{year}.csv`
- Columns: `blank_id` (e.g., `Trip_Blank_1`), `note` (crew + site string)
- To add a new year: create `trip_blank_crews_{year}.csv` -- no script changes needed.

---

## Known Data Issues (Active / Unresolved)

- **WQX/STORET sync (BLOCKED):** 835 2021 records in WQP but absent from WQX Web internal DB.
  CDX batch delete fails. Wait for EPA ETL fix (~April 23). Ready files: in report repo.
- **Characteristic name inconsistency:** Nitrate+Nitrite appears under 3+ names across KWF years.
  Full audit needed (Task 1b).
- **Sample fraction inconsistency:** 2021 dissolved metals submitted as "Filtered, field" -- needs
  re-upload with "Dissolved" (blocked by CDX sync issue).
- **ALS lab duplicates:** 4 results with unexpected DUP status (Task 7) -- does not block CDX.
- **Spring 2013 specific conductance:** values stored as mS/cm, should be uS/cm (Task 12).

---

## Data Storage Structure

```
other/
  input/
    WQX_downloads/              # EPA WQP downloads (excluded from GitHub)
    wqx_templates/              # WQX reference files, lookup CSVs, matching tables
      wqx_qaqc/                 # QA/QC info spreadsheets
      trip_blank_crews_{year}.csv
      {year}_data_flag_decisions.csv
    {year}_wqx_data/            # Raw lab results (SGS, SWWTP, ALS, Taurianen)
      spring_{year}_wqx_data/
        SGS/
        SWWTP/
      summer_{year}_wqx_data/
        SGS/
        ALS/
        SWWTP/
        Taurianen/              # 2021 only; lab closed after 2021
    regulatory_limits/          # master_reg_limits.xlsx + hardness-dependent CSVs
    baseline_sites.csv          # 22 sites: 13 mainstem + 9 tributaries
  output/
    wqx_formatted/              # CDX submission-ready files
      intermediate/             # Pipeline intermediates (not for upload)
    lab_qaqc_data/              # Segregated lab QA/QC results by year
```

**Raw data rule:** Never modify files in `other/input/`. All transformations happen in code.

---

## Project Overview

Long-term cooperative monitoring led by KWF, south-central Alaska. Biannual (spring + summer)
sampling at 22 sites (13 mainstem + 9 tributaries) since 2000.

- **Project home:** https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/
- **GitHub (report repo):** https://github.com/Kenai-Watershed-Forum/kenai-river-wqx
- **GitHub (qaqc repo):** https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc
- **Public data (WQP):** https://www.waterqualitydata.us/ (org: `KENAI_WQX`)
- **CDX submission:** https://cdx.epa.gov/

Primary downstream consumer: **ADEC**, which draws from EPA CDX every two years for the
Integrated Report (impairment decisions).

---

## Key R Packages

`tidyverse`, `dplyr`, `ggplot2`, `lubridate`, `readxl`, `openxlsx`, `writexl`, `DT`,
`janitor`, `data.table`, `stringr`, `hms`, `anytime`, `magrittr`

---

## Useful External Links

- ADEC Integrated Report / CALM: https://dec.alaska.gov/water/water-quality/integrated-report/
- CALM PDF: calm-rev-2021-acc.pdf (in report repo other/agent_context/)
- 18 AAC 70 (Alaska WQ Standards): in report repo other/agent_context/
- WQX web template files: https://www.epa.gov/waterdata/water-quality-exchange-web-template-files
