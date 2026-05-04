# Kenai River WQX — Session Log & Archived Reference

> **Shared context file.** This log is maintained in `kenai-river-wqx` (report repo)
> and auto-synced to `kenai-river-wqx-qaqc` (qaqc repo). It covers work in both repos.
> The qaqc repo prepares and submits annual monitoring data to EPA WQX via CDX; the
> report repo reads that data from EPA's Water Quality Portal and displays it.
> Edit this file only from the report repo (`kenai-river-wqx`).

This file is **not loaded automatically** — reference it on demand when you need history on a completed task, a resolved issue, or the appendix_a.qmd audit. It is the companion to `AGENTS.md`.

------------------------------------------------------------------------

### Session Entry (2026-05-04)

**EPA WQX batch delete — still blocked after ETL restoration.**
- Received Kevin Christian email 2026-05-01 confirming WQP warehouse refresh complete; WQX Last Content Change updated to 2026-05-01.
- Attempted CDX batch delete using `resultphyschem_DELETE_v4.csv`. Result: "Domain Value Invalid" for all 835 Activity IDs (datasetUid=113161). Same error as before ETL fix.
- Root cause confirmed: ETL restoration fixed the WQX → WQP forward pipeline but did not retroactively restore the 835 orphaned 2021 records into WQX Web's internal database. WQX Web still shows zero 2021 records; batch delete requires WQX Web to own the records.
- Follow-up email sent to wqx@epa.gov 2026-05-04. Attachments: `Import Log.xlsx` (from CDX, datasetUid=113161) and `resultphyschem_DELETE_v4.csv`. Requested EPA either (1) delete orphaned records from WQP/STORET directly or (2) restore them into WQX Web so batch delete can proceed.
- Correct DELETE file confirmed: `resultphyschem_DELETE_v4.csv` (835 rows, `ActivityIdentifier` column, no org prefix). v5 is corrupted (837 lines) — do not use.
- `other/epa_cdx.txt` corruption fixed (task notes had been appended to password line); CDX status notes updated to reflect current blocked state.

**GitHub Actions session_log.md sync — set up, not yet pushed.**
- Goal: auto-sync `other/agent_context/session_log.md` from report repo to qaqc repo on every push to main that touches it. AGENTS.md is NOT synced — each repo has its own repo-specific file.
- PAT created: `kwf-report-to-qaqc-sync`, fine-grained, scoped to `kenai-river-wqx-qaqc`, Contents: Read/Write. Secret `QAQC_SYNC_PAT` added to `kenai-river-wqx`.
- Workflow created: `.github/workflows/sync-agent-context.yml` in report repo.
- qaqc repo `AGENTS.md` updated with Repo Relationship section (pulled from remote, prepended, saved locally). `other/agent_context/.gitkeep` added locally.
- session_log.md updated with shared-context blockquote header.
- Report repo `AGENTS.md` Repo Relationship section updated to reflect that only session_log.md is synced (not AGENTS.md).
- **Remaining push steps:**
  1. In `kenai-river-wqx-qaqc`: `git add AGENTS.md other/agent_context/.gitkeep && git commit -m "chore: add repo relationship section and agent context directory" && git push`
  2. In `kenai-river-wqx`: `git add AGENTS.md other/agent_context/session_log.md .github/workflows/sync-agent-context.yml && git commit -m "chore: add repo relationship section and session log sync workflow" && git push`
  3. Verify Action runs in the Actions tab on GitHub (`kenai-river-wqx`).

------------------------------------------------------------------------

## Resolved Known Data Issues

- **Ca/Mg/Fe unit errors: RESOLVED (March 2026).** Summer 2021 SGS EDD reported dissolved Ca/Fe/Mg (method EP200.8) with unit label `ug/L`, but values were on the mg/L scale. Fix applied in `appendix_a.qmd` at the SGS ingestion step: rows where `analyte %in% c("Calcium", "Iron", "Magnesium")` and `units == "ug/L"` are divided by 1000 and relabeled `mg/L`. Spring 2021 had no dissolved Ca/Fe/Mg runs.

- **11 results missing `monitoring_location_id`: RESOLVED (March 27, 2026).** Typo in `sgs_site_names_matching_table_manual_edit.xlsx` — RM 1.5 entry was `RM_1.5_Kenai_City_Dock` (extra underscore) instead of `RM1.5_Kenai_City_Dock`. Fixed by adding one correctly-formatted row to the Excel file.

- **AQWMS/AWQWMS terminology: RESOLVED (March 2026).** All references replaced with `wqx_formatted`/`wqx_templates`. Renamed paths: `other/output/aqwms_formatted_results/` → `other/output/wqx_formatted/`; `other/input/AQWMS/` → `other/input/wqx_templates/`; `AQWMS_template_matching_table.xlsx` → `wqx_template_matching_table.xlsx`; etc. Original vendor file `AWQMS_KWF_Baseline_2021.xlsx` retains its name per raw data policy.

- **HMW station visibility (22 named sites): RESOLVED (April 2, 2026).** `HorizontalCollectionMethodName` corrected from `"Interpolation-Satellite"` to `"GPS-Unspecified"`; `HUCEightDigitCode = 19020302` added to all 22 stations. Corrected `station.csv` uploaded to CDX April 2, 2026. All 22 KWF sites now visible in HMW with monitoring data.

- **`_quarto.yml` corruption: RESOLVED (March 28, 2026).** R code accidentally injected into chapters list; `copper.qmd`, `lead.qmd`, `zinc.qmd` were missing. Restored correctly.

- **Site count 21 vs. 22: RESOLVED (March 28, 2026).** README and AGENTS.md said "21 sites." Correct count is 22: 13 mainstem + 9 tributaries (verified from `baseline_sites.csv`).

- **`appendix_b.qmd` DOCX render failure: RESOLVED (March 28, 2026).** `kable(format = "html")` + `kable_styling()` not guarded by HTML check. Fixed with `if (knitr::is_html_output())` wrapper and plain `knitr::kable()` fallback.

- **AGENTS.md text pasted into `reg_limits.qmd`: RESOLVED (April 2, 2026 evening).** Five separate AGENTS.md blocks had been pasted in at various points. All removed; missing code fences restored; `nitrate_count` code reconstructed from context. File went from 615 to 553 lines.

- **CMA/CMB narrative errors (Q25-Q26 in `appendix_a.qmd`): RESOLVED (April 1, 2026).** Hardcoded prose said CMA = 50.1%, CMB = 52.2% based on a superseded flagging decision. Corrected: CMA = 87.4%, CMB = 92.2% (498/540). All CMA/CMB figures now driven by inline R.

- **Iron threshold not displaying in boxplot: RESOLVED (March 31, 2026).** Root cause: `all_reg_vals.csv` regenerated on every render from `master_reg_limits.xlsx`; direct edits were lost. Fix: added iron row to `static_regulatory_values` sheet in `master_reg_limits.xlsx` (category `total_metals_aquatic_life`, value 1 mg/L), new export block in `reg_limits.qmd`, added CSV to `bind_rows()` in `static_boxplot_function.R`.

- **Water Temp and FC thresholds not surviving renders: RESOLVED (April 2, 2026).** Same root cause as Iron. Fix: added 5 rows to `master_reg_limits.xlsx → static_regulatory_values` (category `field_bio_standards`), new export block in `reg_limits.qmd`, added to `bind_rows()` in `static_boxplot_function.R`.

- **AGENTS.md content pasted into `index.qmd`: RESOLVED (April 20, 2026).** \~20 lines of session notes accidentally pasted after `\newpage` on line 81. Removed.

------------------------------------------------------------------------

## appendix_a.qmd Audit (March 2026)

Line numbers are approximate and shift with editing. As of March 26, 2026 edits, add \~15 lines to original estimates.

| Approx. Lines | Section | Status |
|----|----|----|
| 1–518 | SGS/ALS ingestion (Parts A–E) | Working. Includes Ca/Fe/Mg unit correction. Dense — priority candidate for extraction to `R/ingest_sgs_als.R`. |
| 520–983 | FC and TSS ingestion | Mostly working. TSS has documented QA gap: SWWTP did not report lab QA results in 2021/2022. Lab QA export block commented out. |
| 985–1260 | Lookup joins + first WQX export | Working. Write chunk was `eval = F`, now `eval = T`. `dat_raw`/`dat` save-restore pattern added so WQX CSV is written without corrupting raw-column `dat`. |
| 1268–2410 | QA/QC Checklist (Questions 1–42) | Mostly complete. Gap: Completeness Measure B block is `eval = F` and produces nonsensical results — unfinished (\~line 2141). Q39 (range check) defers to visual review in report chapters. |
| 2411–2533 | Flagging and flagged export | Working. Ca/Mg/Fe unit error corrected upstream at ingestion. |
| 2535–2680 | CDX export (Results and Activities) | Working but nearly duplicates format block at lines 985–1243. Consolidate when extracting to scripts. |
| 2705–2786 | Post-hoc corrections + `knitr::knit_exit()` | Incomplete placeholder. Chunk is `eval = F`. `knitr::knit_exit()` stops rendering prematurely. |

------------------------------------------------------------------------

## Planned File Structure (Future Refactor — Task 10)

Extract processing logic from `appendix_a.qmd` into sourced `.R` scripts for reuse in the qaqc repo.

| Script | Content |
|----|----|
| `R/ingest_sgs_als.R` | SGS EDD + ALS CSV read-in, column normalization, site name mapping, method code mapping (Parts A–E) |
| `R/ingest_fc.R` | SWWTP + Taurianen fecal coliform read-in |
| `R/ingest_tss.R` | SWWTP TSS read-in |
| `R/format_wqx.R` | Lookup joins (lat/long, sample fraction, detection condition, container, preservative), WQX column renaming, Activity ID construction |
| `R/apply_qaqc_flags.R` | Holding time calcs, RPD calcs, completeness measures, flag application, CDX export |

Each script should accept year-specific inputs (file paths, sample dates) as arguments or via a config object.

------------------------------------------------------------------------

## CDX WQX Delete Workflow (Reference)

**WQX Web delete UI path (confirmed working 3/31/2026):** Import & Submit → Import a batch of IDs for records to delete from WQX → Import a file of Activity IDs to be deleted → CSV (Comma delimited). Leave "Ignore First Row of Import File?" checked. Organization ID auto-populated from login.

**Key rule:** DELETE file must contain Activity IDs *without* the `KENAI_WQX-` org prefix. The prefix is added by WQX when storing; do not include it when submitting delete references.

**Correct DELETE file:** `other/output/epa_wqp_uploads/corrected_epa_wqp_uploads/resultphyschem_DELETE_v4.csv` — column `ActivityIdentifier`, 835 rows, no org prefix.

------------------------------------------------------------------------

## HMW Architecture Detail (Confirmed April 1, 2026)

HMW has two distinct display mechanisms:

1.  **Monitoring data** (Past Water Conditions tab): WQP-driven. HMW uses client-side HUC12 spatial operations to find monitoring locations — does not rely solely on stored `HUCEightDigitCode`. Correct station metadata (GPS-Unspecified, populated HUC, correct coordinates) is sufficient.

2.  **Waterbody condition status** (Overview/Aquatic Life/Fishing/Swimming tabs): ATTAINS-driven. Only assessed waterbodies with ATTAINS assessment units appear. HUC 19020302 has ATTAINS assessment units only for the Kenai River mainstem (3 segments) and Kenai Lake. No tributary streams have ATTAINS assessment units — this requires ADEC action in a future Integrated Report cycle (Task 2a).

**Historical numeric-ID stations:** 15 legacy stations (IDs `KENAI_WQX-10000063` through `KENAI_WQX-10000131`, named `KBX_m_*`, 2002–2009, 3,884 results) have `HorizontalCollectionMethodName = "Unknown"` and blank `HUCEightDigitCode`. Corrective file at `other/output/epa_wqp_uploads/corrected_epa_wqp_uploads/station_historical_correction.csv`. Process belongs in qaqc repo (Task 2).

------------------------------------------------------------------------

## EPA WQX Support Call Notes (March 31, 2026 — Kevin Christian, wqx\@epa.gov)

- Current KWF data organized at the "Activity" level (each Activity = one result). Kevin recommends organizing at the "Results" level (each Activity = one sampling event, multiple results nested under it). Performance degrades at scale with current structure. Restructuring uses the WQX Web "Review" function. Address in qaqc repo for future annual submissions (Task 11).

**April 13, 2026 email response re: CDX sync issue:** Root cause confirmed — a failed PostgreSQL instance interrupted the WQX → WQP ETL process. Data added to WQX after the failure may not have fully synced back to WQX Web's internal database. Team working on new PostgreSQL instance; \~two weeks from April 9. Last confirmed WQX content change per WQP portal: 2026-03-19. No action needed from KWF until EPA confirms ETL restored.

------------------------------------------------------------------------

## Session Entries (Newest First)

### Completed (April 21, 2026) — Pipeline Architecture Planning

**Decision: qaqc repo pipeline template (Tasks 18 and 19 created)**

Evaluated several approaches to sharing the QA/QC pipeline between the report repo (`kenai-river-wqx`) and the qaqc repo (`kenai-river-wqx-qaqc`):

- **Rejected:** Shared R package (`kwfbaseline`). Data ingestion varies too much year-to-year for the ingest functions to be reliably packaged. Package overhead not justified.
- **Rejected:** Parameterized sourced scripts only (Option 1). Doesn't address the structural problem of where the canonical template lives.
- **Adopted:** Single-QMD-per-year template, canonically housed in the qaqc repo at `templates/pipeline_template.qmd`. The report repo's `appendix_a.qmd` is the 2021 worked example, not the source of truth.

Template structure: Part A (data ingestion, **inlined**, adapted per year) + Part B (WQX formatting, sourced) + Part C (QA/QC checklist Q1-Q42) + Part D (flag application + CDX export, sourced). Full details in AGENTS.md Pipeline Architecture section.

**2023-specific notes (from `2023.qmd` in qaqc repo — to use when building `2023.qmd` from template, Task 19):**

1.  **Spring SGS EDD split into two CSVs**: `part1` (method 200.7 / EP200.7 results) and `part2` (all other results). Different column types between parts; must be bound with type coercion before main pipeline. Part 1 receive date/time error: all project samples show `"05/09/2023 00:00"` — correct to `"05/09/2023 09:00"` (lab office open hours per COC documents).
2.  **Summer 2023: 200.8 used for both dissolved AND total metals** — no 200.7 present. Distinguish dissolved vs. total using `ANALYSIS_GROUP` column: `"Dissolved Metals by ICP/MS"` (and provisional variant `"Diss. Metals by ICP/MS (Provisional Be,Cu 652023)"`) = dissolved; `"Metals by ICP/MS"` (and provisional variant `"Metals by ICP/MS (Provisional for Be, Cu 06052023)"`) = total/unfiltered. Assign canonical `result_sample_fraction` = `"Dissolved"` and `"Unfiltered"` respectively per canonical scheme.
3.  **No ALS subcontract for 2023**: Spring 2023 200.7 analyses were run by SGS Orlando, not ALS. Results already in the SGS EDD — no separate ALS ingest needed.
4.  **Filter SW846 6010D rows**: These appear in the SGS EDD as project sample records but have no reportable values (document the digestion method only). Remove before pipeline.
5.  **Spring 2023 sample date correction**: Some `sample_type == "PS"` rows have `activity_start_date == "2023-05-03"` — should be `"2023-05-02"`. Correct via `case_when` after ingest.
6.  **TSS receive times (from COC documents)**: Spring 2023 — RM 1.5 received at 15:30, all others at 12:44. Summer 2023 — all received at 12:25.
7.  **Result sample fraction for 2023 dissolved metals**: Use `"Dissolved"` per canonical scheme (AGENTS.md). The `2023.qmd` draft used `"Filtered, lab"` — do not carry that forward.
8.  **Monitoring location ID**: The 2023.qmd draft used a fragile join from a 2021 results CSV, then patched Jim's Landing and Skilak Lake Outflow with `case_when`. Use the proper `sgs_site_names_matching_table_manual_edit.xlsx` approach from the 2021 pipeline instead. A 2023-specific matching table will need to be created in the qaqc repo.

------------------------------------------------------------------------

### Completed (April 16, 2026) — 2025 Preliminary Report + Session

- **Book acronyms chapter expanded.** `chapters/acronyms.qmd` updated from 7 entries to 24, sorted alphabetically. Added: ADEC, BTEX, CDX, EPA, FC, J, KWF, LOD, LOQ, QAPP, QC, RM, RPD, SGS, SWWTP, TSS, U, USEPA, WQP, WQX. Fixed duplicate MDL → MRL.

- **2025 Preliminary Report made year-agnostic.** `other/documents/preliminary_report_2025/2025_preliminary_summary.qmd` refactored so only `report_year <- 2025` at the top needs changing each year. All file paths constructed via `file.path()` and `paste0()`. Cover date auto-generates from `format(Sys.Date(), "%B %Y")`. Note: SWWTP filenames include sampling dates so those path comments require brief manual update each year.

- **Spring SWWTP TSS file format standardized.** Spring 2025 SWWTP TSS file had transposed layout; replaced with new `.xlsx` (`other/input/2025_data/spring_2025/SWWTP/KRWF TSS MONITORING 05-01-25.xlsx`), one sheet (`Updated_Formatting`), 28 rows tidy format. Updated preliminary report QMD to point to `.xlsx`, removed custom `parse_swwtp_sheet()`, spring reading code now matches summer. Note: summer file uses `skip = 1`; spring file does not.

- **Preliminary report render failure fixed.** `report_year` was used before being defined. Fix: added minimal `report-year` chunk immediately after YAML front matter.

- **Preliminary report structural and layout fixes.**

  1.  Acronyms moved to first content section.
  2.  KWF logo DOCX fix: replaced `eval: !expr knitr::is_html_output()` with `::: {.content-visible when-format="docx"}` wrapper.
  3.  HTML layout: added `toc-location: left` and `embed-resources: true`.

- **Preliminary report content and caption fixes.** Renamed "SGS sites" → "Monitoring sites" in Table 1. Removed two em-dashes. Added 2025 raw data download link and KDLL media link. Fixed typo `pe"rformed`. All table captions moved to `#| tbl-cap:` chunk options.

- **FC and TSS incorporated into preliminary report tables.** Added `spring_fc_path`/`summer_fc_path` to path config. Added `parse_fc()` function. Updated `param_labels`, `param_groups`, `results_summary()`, and `tbl-params` to include both analytes. FC files: `KRWF Fecal 04-30-25.xls` (spring), `KRWF Fecal 07-23-25.xls` (summer).

- **YSI RPD join bug fixed.** `ysi_rpd_rows` left join keyed only on `season` and `primary_id` — omitted `ANALYTE`, causing cross-parameter mismatches. Added `ANALYTE` to `select()` and `by =`.

- **DO instrument error excluded.** Soldotna Creek spring DO had primary value of 91.1 mg/L (impossible; almost certainly % saturation). Excluded via `!(Parameter == "DO" & Value > 20)` in `ysi_all` filter block.

- **Table 9 (field duplicate RPD) simplified.** Replaced per-row table with 4-row summary (one per duplicate site): Season, Site, n pairs, n with RPD \> 20%, flagged parameters. Site names normalized via `dup_site_map` named vector.

- **2025 funding note drafted.** `other/documents/preliminary_report_2025/2025_funding_note.docx` (also `.txt`). Several placeholder fields left for manual completion before sending.

- **Executive summary added** as first content section. Uses inline R for dynamic values: site counts, sampling dates, exceedance counts.

- **Library calls moved** to early `libraries` chunk placed after `report-year`. Ensures packages available for RStudio's Render button.

- **Redundant `report_year <- 2025` removed** from `setup` chunk. Now defined only once in `report-year` chunk.

- **Exceedance computation consolidated** into single `exceedances-prep` chunk placed before `## Executive Summary`. Old `exec-summary-calcs` chunk removed.

- **Fieldwork photo added.** `other/documents/images/2025_pictures/IMG_6103.jpeg`, `out-width: "50%"`. HTML-only in title div; DOCX-only on page between TOC and Executive Summary.

- **`pagetitle` updated** to include year: "Kenai River Baseline Water Quality Monitoring: 2025 Preliminary Field Season Summary". Manual annual update alongside `report_year`.

- **Contact info added** to DOCX cover page and HTML header: Benjamin Meyer, Research Coordinator, (907) 260-5449, hydrology\@kenaiwatershed.org.

------------------------------------------------------------------------

### Completed (April 16, 2026 — earlier) — 2025 Preliminary Report Initial Build

Created `other/documents/preliminary_report_2025/2025_preliminary_summary.qmd`. Renders to HTML and DOCX from same source. Key features:

- **Structure:** dual HTML/DOCX format via YAML `format:` block. `{=openxml}` blocks for DOCX cover page, TOC, and landscape sections (ignored in HTML). Conditional `{.content-visible}` divs for format-specific content.
- **DOCX cover page:** KWF logo, bold title/subtitle, PRELIMINARY notice in red, horizontal rule, "Prepared by: Kenai Watershed Forum" block.
- **Page breaks before every `##` section in DOCX.** `other/documents/preliminary_report_2025/pagebreak-h2.lua` — guarded by `if FORMAT == "docx"`.
- **Tables don't split across pages.** `fmt_table()` calls `keep_with_next()` on all body rows.
- **Nitrate/Nitrite missing fix.** Root cause: `filter(ANALYTICAL_CUT == 1)` silently dropped Nitrate/Nitrite (cut 2). Fix: `group_by() |> filter(ANALYTICAL_CUT == min(ANALYTICAL_CUT))`.
- **Bibliography.** `references.bib` with adec-aac-70, adec-aac-80, usepa1976, usepa-nrwqc, adec-qapp.
- **All files in** `other/documents/preliminary_report_2025/`.

------------------------------------------------------------------------

### Completed (April 10, 2026)

- **Quarto render profile dropdown confirmed not an RStudio feature.** GitHub feature request filed Sept 2023, status unknown. Use Terminal: `quarto render --profile docx`, or `quarto::quarto_render(profile = "docx")`. Render instructions added to `other/notes.txt`.

- **"Tributary Sites" / "Main Stem Sites" plot titles added.** `create_plot()` inner function in `static_boxplot_function.R` gained a `title` parameter. No chapter file changes needed.

- **Consistent 2016 report attribution added to all 18 active parameter chapters.** Italic note: *"The following narrative is from the 2016 Kenai River Baseline Water Quality Assessment and reflects data collected through 2014."* 6 chapters standardized; 10 chapters had no note and had it added; 2 partial-update chapters (water_temp, fecal_coliform) got note before historical data summary paragraph only.

- **Annual funding request email template created.** `other/documents/financial/annual_funding_request_template.md`. Placeholders: `[Name]`, `[$AMOUNT]`, `[Agency Name]`, and two inline links to update annually.

------------------------------------------------------------------------

### Completed (April 9, 2026)

- **Reorganize Quarto project: move chapter `.qmd` files to `chapters/` subfolder.** 19 `.qmd` files moved. `index.qmd` stays at root. `_quarto.yml` updated with `chapters/` prefix. Image paths in `study_area.qmd` (25 images) and `interpreting_boxplots.qmd` (1 image) updated from `other/...` to `../other/...`. R code paths unaffected (`execute-dir: project`).

- **Diagnosed CDX batch delete failure (Task 1a-reupload).** Root cause: 835 2021 records in WQP/STORET but absent from WQX Web internal DB. WQX Web Review shows zero 2021 records; CDX batch delete returns "Domain Value Invalid." DELETE file is correct (verified). **Email sent to wqx\@epa.gov April 9, 2026.** See EPA response in CDX WQX Delete notes above.

------------------------------------------------------------------------

### Completed (April 8, 2026)

- **2021 CDX batch delete attempts (v3–v5) — all failed.** v3: wrong column name (`Activity ID` instead of `ActivityIdentifier`). v4: correct column name but same error as v2 (which worked 3/31) — WQX-side issue. v5: file corrupted (837 lines instead of 836). `resultphyschem_DELETE_v4.csv` is the correct file to use when issue is resolved. Do not use v5.

- **Output files confirmed ready (no re-render needed).** `results_activities.csv` (April 6, 20:11): correct sample fractions. `station.csv`: GPS-Unspecified + HUC code. `project.csv`: unchanged.

------------------------------------------------------------------------

### Completed (April 6, 2026)

- **Task 1a: Standardize sample fraction names in 2021 pipeline.**

  1.  `wqx_template_matching_table.xlsx → result_sample_fraction` sheet: dissolved metals (method 200.8) changed from `"Filtered, field"` → `"Dissolved"`.
  2.  `appendix_a.qmd`: added explicit `mutate()` for TSS (`"Suspended"`) and FC (`"None"`).
  3.  `other/documents/sample_fraction_correction_handoff.md` created for qaqc repo handoff.

- **Historical years (2000–2013):** dissolved metals already use `"Dissolved"` — no re-upload needed. Ca/Fe/Mg `Total Recoverable` → `Unfiltered` correction addressed in qaqc repo via handoff doc.

- **2023+ context:** Lab switched from field to lab filtration for dissolved metals. `"Dissolved"` correctly applies to both. Method alone no longer distinguishes dissolved from total — fraction field is only distinguishing factor.

------------------------------------------------------------------------

### Completed (April 3, 2026)

- **Planned duplicate RPD summary table for `data_qa_qc.qmd`** (not yet implemented). See Task 17 in AGENTS.md for full implementation approach.

------------------------------------------------------------------------

### Completed (April 2, 2026 — evening)

- **Cleaned AGENTS.md text accidentally pasted into `reg_limits.qmd`.** Five separate blocks removed; missing code fences restored; `nitrate_count` code reconstructed. File went from 615 to 553 lines.

- **Fixed HTML plotly legend group subtitles.** Root cause: `legendgrouptitle` was being set on every trace in each group. Fix: `group_titled` list tracks whether each group's subtitle has been applied; set only on first visible trace, `list(text = "")` on all subsequent. Result: "Static regulatory thresholds" and "Hardness-dependent criteria" each appear exactly once. Verified against Copper.

- **Reviewed ADEC CALM document (`other/agent_context/calm-rev-2021-acc.pdf`).** Key findings:

  - Assessment Level minimum: 10 samples (5 for toxics) over 2+ years within most recent 5-year period.
  - For 10–11 samples, 2 exceedances needed to list as impaired.
  - FC, turbidity, and petroleum hydrocarbons explicitly excluded from standard CALM methodology.
  - Grab samples may be treated as representative of averaging periods (Section 3.2).
  - New tasks added: 1c (5-year sample count check) and 5a (CALM methodology notes).

------------------------------------------------------------------------

### Completed (April 2, 2026)

- **Re-upload corrected `station.csv` to CDX.** `HorizontalCollectionMethodName = "GPS-Unspecified"`, `HUCEightDigitCode = 19020302` for all 22 stations. Pipeline durability confirmed: fix hardcoded in `appendix_a.qmd` station generation block; `case_when` catches both `"Interpolation-Satellite"` and `"Unknown"`. `WaterbodyName` is not importable via WQX Web CSV batch import UI (field not in Elements list); set manually via WQX Web Review only.

- **Moved `standard_labels`/`standard_authority` from hardcoded R to `master_reg_limits.xlsx`.** Added `standard_types` sheet with 20 rows. Updated `threshold_table.R` to read from sheet. Six rows flagged `review_needed = Y` (Task 5).

- **Fixed Water Temp and FC threshold values.** Added 5 rows to `master_reg_limits.xlsx → static_regulatory_values` (category `field_bio_standards`). New export block in `reg_limits.qmd`. Added to `bind_rows()` in `static_boxplot_function.R`.

- **Revised boxplot legend.** `std_labels` moved to top level of `static_boxplot_function.R` (computed once on source, shared). `linetype_map` expanded to cover all 20 known standard type codes. `scale_linetype_manual()` uses `labels = std_labels[present_standards]`. Legend sub-titles: "Static regulatory\nthresholds" and "Hardness-dependent\nexceedance type".

------------------------------------------------------------------------

### Completed (April 1, 2026)

- **Corrected CMA/CMB narrative errors in `appendix_a.qmd` (Q25–Q26).** CMA: 50.1% → 87.4%. CMB: 52.2% → 92.2% (498/540). CMA by site range: 44.4%–71.4% → 80.6%–92.1%. Added `cma-narrative-values` and `cmb-narrative-values` chunks — all figures now driven by inline R.

- **Converted hardcoded numerical prose values to inline R.** Q18 (metals comparison), Q19 (RPD analysis — revealed several inaccuracies in old hardcoded values), Q20 (matrix spike recovery range).

- **HMW visibility investigation concluded.** See HMW Architecture Detail section above.

- **Refactored parameter chapter boilerplate via `render_plots.R` + `knit_child()` template.**

  1.  `functions/render_plots.R`: defines `render_parameter_plots(plots)` — returns `htmltools::tagList` for HTML, calls `print()` for DOCX.
  2.  `templates/_parameter_chunk.Rmd`: knitr child template with four source+call pairs.
  3.  Each chapter now sets `characteristic` (+ optional `sample_fraction`, `no_threshold_note`), then calls `knitr::knit_child()` with `results='asis'`.

------------------------------------------------------------------------

### Completed (March 31, 2026)

- **CDX WQX delete file debugging.** Root cause of initial failure: DELETE file contained Activity IDs with `KENAI_WQX-` org prefix. WQX expects IDs without org prefix. Corrected file (`resultphyschem_DELETE_v2.csv`) successfully deleted all 835 existing 2021 records on 3/31/2026.

- **2021 CDX re-upload completed.** All three files uploaded. WQP download 3/31/2026 confirms 835 2021 records present.

- **Station ID coverage analysis.** WQP has 37 total station IDs for KENAI_WQX: 22 named IDs (`KBL_m_*`, 12,556 results, 2000–2021) and 15 legacy numeric IDs (`KBX_m_*`, 3,884 results, 2002–2009 only). Corrective file: `other/output/epa_wqp_uploads/corrected_epa_wqp_uploads/station_historical_correction.csv`.

- **`water_temp.qmd` narrative updated.** Added three ADEC temperature standards (20/15/13°C) with 18 AAC 70 attribution. Note added to evaluate alongside continuous logger records in AKTEMP.

- **`fecal_coliform.qmd` narrative updated.** Geometric mean cannot be evaluated (only two events/year). Single-sample limits (400 and 40 CFU/100 mL) are applicable comparisons.

- **`iron.qmd` citation corrected.** `(ADEC, 2008; USEPA, 2014)` → `(ADEC, 2008; USEPA, 1976)`. Iron criterion originates from USEPA 1976 Red Book.

------------------------------------------------------------------------

### Completed (March 28, 2026)

- **Integrated `project.csv` and `station.csv` generation into `appendix_a.qmd`.** Previously commented-out block now runs automatically. Deleted `other/output/wqx_formatted/final_preparation/` folder.

- **Cleaned up redundant output files in `wqx_formatted/`.** Deleted stale `2021_kwf_baseline_results_aqwms.csv`. Moved two pipeline intermediates to `other/output/wqx_formatted/intermediate/`. Updated all 18 path references in `appendix_a.qmd`.

- **Added per-chapter regulatory threshold tables.** `functions/threshold_table.R` with `show_threshold_table(characteristic)`. Added `source()` + call to all 19 active parameter chapters. Regulatory authority mapping reads from `master_reg_limits.xlsx → standard_types`.

- **Restored out-of-range threshold legend entries.** Removed `hline_visible` filter from `static_boxplot_function.R`.

- **Tooltip: prefix "RM" to mainstem river mile labels.** One-line change in `static_boxplot_function.R`.

- **Enabled DOCX render.** Created `functions/embed_if_html.R` helper; replaced all 32 `xfun::embed_file()` calls. Wrapped two leaflet maps and one ggplotly in `if (knitr::is_html_output())` conditionals. Fixed `rm(list=ls())` clearing the function by adding `source("functions/embed_if_html.R")` after the `rm()`. Added `cache: false` to `appendix_a.qmd` YAML.

- **Fixed code print leakage in `data_sourcing.qmd`.** YAML front matter was placed after the heading on line 1 (not parsed as front matter). Fixed by moving YAML to top of file. `knitr::knit_exit()` chunk also lacked `echo = FALSE`.

- **Integrated 2016 report site descriptions into `study_area.qmd`.** Converted 2016 DOCX to markdown; extracted 22 embedded site photos to `other/documents/site_photos/`. Added text, coordinates, photo, and caption for all 22 sites.

- **DOCX formatting: suppress code output globally.** `execute: echo/warning/message: false` added to `_quarto-docx.yml`.

- **DOCX formatting: chapter page breaks.** `filters/pagebreak-h1.lua` — inserts OpenXML page break before every H1; guarded by `if FORMAT == "docx"`.

- **Separated HTML and DOCX output directories.** `project: output-dir: docs-docx` in `_quarto-docx.yml`. HTML → `docs/`; DOCX → `docs-docx/`.

- **Set up Quarto render profile files.** `_quarto-html.yml` and `_quarto-docx.yml`. `profile: default: html` and `group: - [html, docx]` in `_quarto.yml`. Confirmed profile dropdown is not an RStudio feature (April 10, 2026).

------------------------------------------------------------------------

### Completed (March 27, 2026)

- **Fixed 11 results with missing `monitoring_location_id`.** See Resolved Known Data Issues above.

------------------------------------------------------------------------

### Completed (March 26, 2026)

- **Re-render `appendix_a.qmd`.** Two bugs fixed: (1) main WQX column-formatting/write chunk had `eval = F` — changed to `eval = T`; (2) enabling that chunk broke downstream QA/QC chunks. Fixed by saving `dat_raw <- dat` before WQX mutate and restoring `dat <- dat_raw` afterward.

- **Replaced AQWMS/AWQWMS terminology.** See Resolved Known Data Issues above.

------------------------------------------------------------------------

### Completed (March 25, 2026)

- **Inspected and corrected Ca/Mg/Fe unit errors in 2021 data.** See Resolved Known Data Issues above.
