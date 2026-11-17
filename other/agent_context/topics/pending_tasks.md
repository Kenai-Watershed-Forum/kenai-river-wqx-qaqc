# Pending tasks and next steps (detail)

Moved out of `AGENTS.md` 2026-11-16 to keep core memory short. `AGENTS.md` holds the short list; this file holds the full text. Revisit at the start of each session.

## Phase B remainder
Phase B: done 2026-11-13 (`combine_dat.R` runs the ingests and the contract check; lookup sheets cover SW8260D, 2540-D, 9222D and the YSI methods; lookup check is now an error; `in_situ_methods.csv` marks YSI as having no container or preservative). Left: CDX lessons step 3 remainder (Method Speciation, characteristic-name mapping) waits on the historical data scan. Unconfirmed: whether WQX accepts `Probe/Sensor` as Equipment ID. FC container color `Clear` confirmed; TSS preservative left blank (optional field).

## Phase C (qaqc.qmd) full list
Phase C: `qaqc.qmd`. Done: field blank coding (`activity_coding.R`), `assess_lab_qc.R` (Q20), Q19 RPD (`assess_rpd.R`: QAPP v3 primary, ADEC 2026 beside it), `dl` column added to `dat` 2026-11-16 (SGS/FC/TSS; `format_wqx.R` exports it as Method Detection Level, Type 2 blank when NA). NEXT: (a) `assess_lab_qc()` blank rule: QAPP (any non-U detection) primary, ADEC (>= LOQ) beside it, using `loq`/`dl`; (b) write `assign_qc_status()` (design in `topics/known_issues.md`: Result Status ID only Accepted/Rejected; fail carried in Result Qualifier `QCF` and Result Comment `QC FAIL: <reason>`; lab QC fails reject same-batch, same-analyte samples; blank scope uses a 5x-blank convention pending user OK; FC/TSS: one batch per lab, analyte and season; needs `sample_batches` side table from `ingest_sgs`); (c) `reference/qualifier_definitions.csv` (as complete as possible) and a data usability statement for the report template (`topics/adec_and_docs.md`); (d) `qaqc.qmd` skeleton (build runnable skeleton first; write flagged export to `assessed_results.csv`), Q17 fixes. Open: LLQC has no acceptance limits (ask SGS; Q20 text must say LLQC not evaluated); SGS LOD is 0.75 x LOQ in the EDD vs 1/2 in SGS note and QAPP (ask SGS); QAPP completeness goal vs ADEC 80-90% (check before Q25/Q26); WQX acceptance of project qualifier code and `Limit of Quantitation` vs `Lower Quantitation Limit`. Since spring 2026 the City of Kenai plant runs all TSS. When the template design is complete, delete every `project_year/####.qmd`; keep only the year-agnostic template.

## Order agreed with the user (2026-11-16)
1. `assess_lab_qc()` blank rules (QAPP non-U primary, ADEC >= LOQ beside it; tests).
2. `assign_qc_status()` plus `sample_batches` side table; `generate_cdx_export.R` builds Result Status ID (Accepted/Rejected), Result Qualifier (`QCF`) and Result Comment from `qc_status`/`qc_fail_reason`. Confirm the blank multiple (5x proposed; user may choose 10x).
3. `reference/qualifier_definitions.csv` (QAPP Table 7, values in data, WQX detection conditions, `QCF`; separate tables for QC sample types and status values) and a data usability statement for `prelim_report.qmd`.
4. Completeness goal check (QAPP vs ADEC 80-90%) before Q25/Q26.
5. `qaqc.qmd` runnable skeleton, then port Q1-Q42 in batches; Q17 rebuild; Q19 text says the project operates under QAPP v3 (updated about every 5 years), ADEC 2026 shown beside it; Q20 text says LLQC not evaluated and the blank multiple is a project convention.
6. Ask SGS: LLQC limits; LOD 0.75 x LOQ vs 1/2; blank criterion.
7. Report-repo task: add an outdated note to appendix_a; later re-run 2021 with this template to replace CDX content.
8. Trial WQX import to settle: `Probe/Sensor` Equipment ID, free-form `QCF` qualifier, `Limit of Quantitation` vs `Lower Quantitation Limit`.

## Historical data scan (moved from AGENTS.md)
**Pending: historical data scan (2026-11-09).** Before CDX lessons step 3, the user wants all 2014-2024 data scanned (read-only) to learn what the template must handle: labs and layouts, site-label spellings (`resolve_rm_site`, SGS name table), method codes and units against the lookup sheets, non-numeric text (`to_numeric_strict`), gaps in the 24-column contract. Output: a gap table by year. The user arranges `other/input/<year>/` and says when ready; prompt for it. Open questions: compare against 2025 as baseline; include the 2013-2017 lab-measurement workbooks. The PDF extraction work (`topics/pdf_extraction.md`) prepares the PDF-only years.
