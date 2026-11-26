# Kenai River WQX QAQC: Project Memory (core)

Core memory loads every session; keep it short (target 11 KB). Details live in `other/agent_context/topics/` (read the relevant file on demand, see "Topic index"). Pre-split original: `other/agent_context/archive/AGENTS_2026-11-13_pre-split.md`. Put new detail in a topic file, not here; put test-fixture counts and validation narratives in the session log or topic files.

## Next steps (updated 2026-11-25)

1. Phase B done 2026-11-13. Left: CDX lessons step 3 remainder (Method Speciation, characteristic-name mapping; waits on the historical scan); WQX trial import questions. Detail: `topics/pending_tasks.md`.
2. Phase C (`qaqc.qmd`) in progress; full render works end to end (`quarto render qaqc.qmd`, output `output/report/qaqc.html`). Q17-Q35 done (run order: combine_dat, assess_lab_qc, assign_qc_status, assess_holding_times, format_wqx, apply_qaqc_flags, generate_cdx_export). Detail: `topics/pending_tasks.md` (sections "Session 2026-11-25", "Q33", "Q34", "Q35") and `topics/pending_tasks_done.md`. Q33-Q35 validate against EPA WQX domain lists vendored in `reference/wqx_domains/` (helper `functions/check_wqx_domain.R`; online file names are internal domain names, e.g. `ThermalPreservativeUsed`; list in pending_tasks Q34 section); WQX names come from the `wqx_characteristic_name` column of `analytes_list_manual_edit.csv`; WQX constants live in `config.yml` `wqx:`. User decision: keep `Field Msr/Obs` and `Quality Control Field Replicate Msr/Obs` activity types. NEXT, strictly in order, one question at a time, never skip: Q36-Q42 (design in pending_tasks; Q37 relative-depth decision pending), then Q3 (QC sheet vs `dat`) and Q1-Q9 (mostly manual prose). Open: the CDX step (generate_cdx_export) is silently skipped because `other/input/WQX_downloads/` is missing, so `results_activities.csv`, `project.csv`, `station.csv` and the two intermediate CSVs are STALE (do not upload; make the skip a loud warning); FC/TSS receipt date/time in `config.yml` are blank; the 2025 flag-decision row for dissolved lead is a test placeholder. Flag decisions: `flag` is trimmed and case-insensitive. Where we interpolate from the QAPP, say so in the report text and add a row to `topics/qapp_revision_notes.md`. Delete `project_year/####.qmd` once the template design is complete.
3. Historical reconcile (2014-2018 PDF workbooks) waits on the user's Decision 2 (write WQX names into `Characteristic Name`, recommended, or project names); open decisions are listed in `topics/pdf_extraction.md`.

## Repo Relationship and Purpose

Two repos form the Kenai River Baseline Water Quality Monitoring pipeline (Kenai Watershed Forum, KWF):

- **`kenai-river-wqx-qaqc`** (this repo, https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc): a **single-year template**. It takes any one project year's raw lab files through ingestion, QA/QC, WQX formatting and CDX export, and produces a draft (preliminary) report like the 2025 one. It repeats the process that `appendix_a` documents in the report repo.
- **`kenai-river-wqx`** (report repo, https://github.com/Kenai-Watershed-Forum/kenai-river-wqx, stays active and standalone): a Quarto book on long-term trends. It reads from EPA's Water Quality Portal (WQP), not from this repo. Its `chapters/appendix_a.qmd` is a worked QA/QC example and the canonical text for the 42 QA/QC questions.

Flow: this repo submits to EPA WQX via CDX, EPA publishes to WQP, the report repo reads WQP. The same data feeds the EPA How's My Waterway map. ADEC accesses the data from WQP (no separate ADEC submission; see `topics/adec_and_docs.md`).

**Pending: historical data scan (2026-11-09).** Before CDX lessons step 3, the user wants all 2014-2024 data scanned read-only to learn what the template must handle; output is a gap table by year. The user arranges `other/input/<year>/` and says when ready; prompt for it. Detail: `topics/pending_tasks.md`.

**Divergence:** the report repo keeps its own copies of the ingest and format code in `functions/appendix_a_scripts/` (copies in `other/misc/report_repo_scripts/`). Their relation to `functions/` here is undecided; fixes in one are not in the other.

**`other/agent_context/session_log.md`** is auto-synced from the report repo and covers both repos. Edit it only from `kenai-river-wqx`.

Design record: `other/agent_context/plans/2026-10-28-qaqc-repo-as-parameterized-template.md` (rev. 2; read before architectural changes). Its archive/fetch/checksum content and item 13 are out of date; the blank-template model below supersedes them. This repo is not archived.

## Working Model

**This repo is being built into a blank, robust template that works for any year.** For each project year the user copies the template into a **new GitHub repo** and puts only that year's raw lab files in `data/` (committed there; one year per repo). 2025 is the test fixture. The template must run the full QA/QC process and draft the preliminary report.

- Function code contains nothing year-specific. Year-specific values live only in `config.yml`, `year_files/` and reference tables.
- At the end, 2025 data and outputs are removed (placeholders, `data/.gitkeep`, README new-year checklist), the repo is marked a GitHub template, and a trimmed example folder (suggested `example_2025/`, contents undecided) stays. Legacy `project_year/2014-2024` and `other/input/<year>` are also removed.
- Archive and fetch mechanics were deleted (2026-10-28); `README.md` and the plan file still describe them and need rewriting.

**Phases** (each ends with a check-in; present decisions one at a time):

| Phase | Content | Status |
|----|----|----|
| A | Contract, layout | Largely done. Remaining: reconcile function copies with the report repo (deferred) |
| B | Per-lab ingest functions; surrogates to `lab_qc_dat`; `mg/L` guard; contract check | In progress. Ingest functions (SGS, FC, TSS, YSI), `clock_utils.R`, `check_dat_contract.R` and `dat_contract.md` done and verified. Open items: see Next steps |
| C | `qaqc.qmd` Parts B-D; field blanks; Q20 surrogate check; Q17 fixes; `assessed_results.csv`, `lab_qc_assessed.csv` | Pending |
| D | `prelim_report.qmd` generalized from `other/documents/preliminary_report_2025/2025_preliminary_summary.qmd`; its setup chunk (about lines 200-530) is replaced by reading `assessed_results.csv` | Pending |
| E | Blank the template, keep trimmed example, update both repos' docs. GitHub Pages is live from `main` `/docs` (old book snapshot, frozen since `_quarto.yml` writes to `output/report`): leave `docs/` until Phase E, then unpublish Pages in Settings and delete `docs/`, `project_year/`, `index.qmd`, `intro.qmd`, `summary.qmd`, `cover.png` | Pending |

Key decisions: `dat` has 26 columns (24 plus `dl`, added 2026-11-16, plus `analytical_method_raw`, added 2026-11-24; `reference/dat_contract.md`); field blanks use Activity Type `Quality Control Sample-Field Blank` and Activity ID suffix `FB`; `lab_qc_dat` holds all 14 SGS lab QC codes plus any `(surr)` analyte and never flows to the WQX export; `prelim_report.qmd` reads only `output/assessed_results.csv`, `config.yml` and reference data.

**Decisions 2026-11-25** (completeness Measures A/B, `baseline_sites.csv` as the single coordinate source, `sampled_locations.csv`, CDX project, organization, QAPP and time-zone settings in `config.yml` `cdx:`): see `topics/pending_tasks.md`, section "Session 2026-11-25".

## ID rule

Never include the `KENAI_WQX-` prefix in any ID column (WQX Web prepends it; WQP downloads already include it). Upload files use bare IDs: Location ID `10000002`; Activity ID `{site_code}-{date}-{abbrev}[-DUP/-Blank]`. `format_wqx.R` builds the IDs once; `check_cdx_ids()` stops the export on any violation. History, the 22-site ID table and the lookup sheets: `topics/lookups_and_ids.md`.

## Current Layout

The pipeline is mid-migration: working pipeline today is `project_year/2025.qmd`; target is `qaqc.qmd` plus `prelim_report.qmd`.

```
kenai-river-wqx-qaqc/
├── config.yml                  # Year settings (dates, lab file paths/sheets, FC/TSS run and receipt date+time, sgs trip_blank_sites, tss lod/loq, ysi limits)
├── data/                       # One year's raw lab files (committed per year repo)
├── functions/                  # ingest_sgs/fc/tss/ysi.R, resolve_rm_site.R, clock_utils.R, numeric_utils.R, check_dat_contract.R,
│                               # check_cdx_ids.R, check_cdx_fractions.R, format_wqx.R, apply_qaqc_flags.R, generate_cdx_export.R,
│                               # pdf_extract/ (historical PDF tools)
├── reference/                  # dat_contract.md, wqx_templates/ (lookup sheets), baseline_sites.csv, ysi_site_names.csv,
│                               # master_reg_limits.xlsx, formatted_reg_vals/
├── year_files/                 # Working-year: flag decisions, ysi_corrections.csv, trip_blank_crews, planned_samples.xlsx, sampled_locations.csv, AWQMS file
├── output/                     # Working-year outputs, intermediate/
├── project_year/               # 2025.qmd active; 2023.qmd skeleton; 2014-2022 legacy
├── other/                      # agent_context/ (session_log.md synced, plans/, topics/, archive/), documents/ (md/ QAPP, CALM, 18 AAC 70;
│                               # wqx_documents/; preliminary_report_2025/; cdx_upload_history/), misc/ (report_repo_scripts/ for reference,
│                               # qaqc_repo_transition/ stale, do not edit), input/ (legacy per-year inputs, WQX_downloads/), output/
├── tests/                      # testthat files for the functions
├── _quarto.yml                 # book; output-dir output/report
└── README.md                   # STALE; to be rewritten as a copy-the-template checklist
```

Governance PDFs stay in the report repo's `other/agent_context/`; text versions are in `other/documents/md/`. The WQX Web import walkthrough (Uid 9481) is `cdx_upload.qmd`; working config `other/documents/EPA_WQX_templates/cdx_import_config_results_activities_20261028.cfg`.

## Topic index (read on demand)

| File | Read when |
|----|----|
| `topics/ingest_and_contract.md` | Touching ingest functions, `check_dat_contract`, `format_wqx.R`, `apply_qaqc_flags.R`, units rules, or the CDX lessons plan (steps 1-6) |
| `topics/pdf_extraction.md` | PDF extraction, v2 workbooks, 2014-2018 reconcile design and its open decisions |
| `topics/lookups_and_ids.md` | Lookup sheets, the 22 location IDs, ID guard history |
| `topics/adec_and_docs.md` | ADEC/Integrated Report context, README checklist plan, book and docs |
| `topics/known_issues.md` | Known issues, QA/QC question numbering and wording |
| `topics/qapp_revision_notes.md` | Porting any QAPP-based question; every QAPP interpolation gets a row (for the next QAPP update) |
| `topics/pending_tasks.md` | Start of every session and after a compact: full next steps, open questions, agreed order |
| `archive/legacy_2025qmd.md` | Only when porting or reading the old inline `2025.qmd` pipeline or `2023.qmd` |

## Report-Repo Items (tracked in `kenai-river-wqx`, not done here)

2021 WQP verification, cache refresh and book render; trend analysis rollout; characteristic name audit (Task 1b); CALM 5-year sample counts (Task 1c); `review_needed` verification in `master_reg_limits.xlsx` (Task 5). See that repo's `AGENTS.md`.

## Style Preferences

- Git: never commit or push; the user does it manually. Remind the user when a commit is due (after finished Qs or cleanups).
- Token use: proactively suggest the `explore` subagent (launchSubagent) for broad file or data discovery (e.g. the historical scan) and suggest `/compact` or a fresh session after each finished Q. The user is new to subagents. Do not read `archive/` or `session_log*.md` unless asked.
- This repo is a blank template for any year. Never record or code row counts, site counts or other 2025 values as expectations; label 2025 numbers as test-fixture results. Checks test structure and rules (columns, types, units, allowed values, config consistency).
- Do not put conversation-title markup literally in `/compact` summaries (it truncated a summary). Approval of a design is the go-ahead to write it.
- No em-dashes; use a colon, comma, semicolon or parentheses. Minimal AI-writing tropes.
- All ingest scripts must emit `mg/L` (WQX capitalization).
- Do not interpret trends in text.
- Pipes: base `|>` only; no magrittr (`%>%`, `%<>%`; use `x <- x |> f()`). Live code cleaned 2026-11-24; `project_year/` and `other/misc/` copies are legacy and left as is. The repo moved out of OneDrive to `C:\_github\kwf\kenai-river-wqx-qaqc` (2026-11-24); if another PC or tool rewrites files as CRLF, re-strip CRs before editing.
- Lessons learned: each year repo keeps `other/agent_context/lessons_learned/{year}_lessons_learned.md` (format in `other/agent_context/lessons_learned/_TEMPLATE_lessons_learned.md`). When the user points to one, follow `other/agent_context/lessons_learned/README.md` to triage and apply it to the template.
- Line endings: all text files are LF (`.gitattributes` `text=auto eol=lf`, `.editorconfig`; normalized 2026-11-24). The edit tool works normally. If an edit fails on a multi-line match, check the file for CRs (`git ls-files --eol`).
- When running testthat or readxl from the R session, use `reporter = "silent"` and `as.data.frame(results)` to avoid progress-bar noise.
- Before implementing code, explain what it does and why, and wait for confirmation. Present decisions one at a time.
- Work through the QA/QC questions in numeric order and do not skip any, including manual ones (give each a MANUAL prompt). Everything must stay year-agnostic: 2025 is only a fixture.
- Edit-tool caution (2026-11-25): a qaqc.qmd edit once landed at the top of the file and broke the YAML. After each qaqc.qmd edit, check `head -5 qaqc.qmd`; verify by rendering. Back up to the scratch dir before any bulk deletion; the agent runs `quarto render qaqc.qmd` from bash to test.
- Corrections to hand-entered data go in a separate `<name>_corrected.xlsx` beside the original (changes shaded, logged on a `corrections_log` sheet); never modify the original. Keep the key's own conventions unless the user decides otherwise.
