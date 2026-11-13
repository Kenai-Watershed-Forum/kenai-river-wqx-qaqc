# Known issues and QA/QC question numbering

Moved verbatim from AGENTS.md on 2026-11-13. Read when relevant; core memory is AGENTS.md.

## Known Issues / Flags

**OPEN TASK: LLQC acceptance limits (Q20).** SGS EDDs give LLQC (low-level quantitation check) rows a recovery but no recovery limits, and OS (original sample) rows neither. `assess_lab_qc()` marks them `Not evaluable`, never `Pass`. For LLQC this is a gap: performance near the reporting limit is not assessed. OS rows are unspiked parents, so `Not evaluable` is correct for them. Follow-up: ask SGS whether they apply LLQC acceptance limits (and which); also check the QAPP QC tables (no numeric LLQC limit was found in a keyword search of `other/documents/md/`). If limits exist, add a reference table (method, analyte, low, high) that `assess_lab_qc()` reads where the EDD limits are blank. Until then, the Q20 section of `qaqc.qmd` must state that LLQC rows were not evaluated.

Contract violations found in the 2025 data (to be fixed in Phases B and C):

- **Units case:** 48 SWWTP TSS rows carry `mg/l` in `2025.qmd`, which hardcodes it. Fixed in `functions/ingest_tss.R`; `2025.qmd` itself still has the problem.
- **Surrogates leaking:** 24 BTEX surrogate recovery rows (units `%`) are in `dat` and in the 2025 WQX export as `Field Msr/Obs`. They belong in `lab_qc_dat`.
- **Fixed 2026-11-13:** field blanks coded (Activity Type `Quality Control Sample-Field Blank`, ID suffix `-FB`) and the dead `"Blank"` branch removed, via `functions/activity_coding.R` (tests: `tests/test_activity_coding.R`). On the 2025 fixture `check_cdx_ids` now passes.
- **Duplicate Activity IDs:** 116 rows in the 2025 output share an Activity ID with the same characteristic, fraction and method. Not caused by the ID change; investigate before upload.
- **Duplicated spring and summer ingest chunks** (resolved for SGS, FC, TSS and YSI by the new functions).
- **YSI receipt:** `2025.qmd` copies collect time into `rec_date`/`rec_time`; should be NA (fixed in `ingest_ysi.R`).
- **Citations:** `index.qmd` cites `@mccard2007` and `@guerronorejuela2016`, missing from `references.bib`.
- **Stale docs:** `README.md`, the plan file (archive/fetch content, item 13) need updating (`reference/dat_contract.md` was refreshed 2026-10-28; recheck it once Phase C settles). Defer until code settles. Small items: the `config.yml` `tss:` comment cites `SM21-2540-+D` while the function emits `2540-D`; the TSS/FC comment lines in `config.yml` sit above the `fc:` block. `2025.qmd` still hardcodes TSS `mg/l` and the 0.5 LOD.

Other:

- **Function copies differ** from the report repo's `functions/appendix_a_scripts/` (large diffs in `format_wqx.R` and `generate_cdx_export.R`). Reconcile in Phase B; the report repo's `generate_cdx_export.R` has the prefix strip and the `na = ""` fix, plus the characteristic-name mapping and Method Speciation (`as P`, `as N`) that are not yet ported here (plan step 3). The `check_cdx_ids()` guard exists only here.
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

