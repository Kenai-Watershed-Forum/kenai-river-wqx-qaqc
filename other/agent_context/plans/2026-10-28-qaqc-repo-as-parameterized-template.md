# QA/QC Repo as a Single-Year Template: Revised Plan (rev. 2, 2026-10-28)

Status: APPROVED 2026-10-28; layout, phases, and decisions rewritten the same day after
the decisions below. Phase A in progress (step 1: `reference/dat_contract.md` drafted in
the qaqc repo). Supersedes Phase 2 and Phase 3 of
`2026-10-14-1300-per-year-repo-architecture-migration.md` (Phases 0 and 1 stand).

## Concept

`kenai-river-wqx-qaqc` is a **template repo for one data year at a time**. When a year is
being worked, the repo holds only that year's raw lab files, config, outputs, and
preliminary report, plus shared code and reference data. A finished year is archived as a
GitHub Release "year bundle" and removed from the working tree. Years are developed one at
a time, starting with 2025 and working backwards (2024, 2023, ...).

Because intake and output handling need customizing and troubleshooting for each year,
the working copy of the pipeline is edited directly for the current year. General fixes are
promoted back into the template at archive time.

## Decision changes from the 2026-10-14 plan

| Old decision | New decision |
|---|---|
| New repo `kenai-river-baseline-wqx-template` | Not created. `kenai-river-wqx-qaqc` is the template. |
| One repo per data year | One repo, one working year at a time; past years archived as release bundles. |
| Archive `kenai-river-wqx-qaqc` | Not archived. Active home for QA/QC and the preliminary report. |
| Preliminary report generalized in a new repo | Same idea, lives in `kenai-river-wqx-qaqc`. |

Unchanged: `kenai-river-wqx` (report repo) is standalone, reads only from WQP, and does not
depend on the qaqc repo (only `session_log.md` is synced by Action). Shared reference data
(`master_reg_limits.xlsx`, `baseline_sites.csv`, WQX matching tables) are single copies in
the qaqc repo.

## Target repo layout (`kenai-river-wqx-qaqc`), working year only

```
qaqc.qmd                  # QA/QC pipeline for the working year (Parts A-D); edited per year
prelim_report.qmd         # narrative report; reads only assessed_results.csv
config.yml                # working year: dates, input file names, sheet/skip settings
functions/
  ingest_sgs.R  ingest_fc.R  ingest_tss.R  ingest_ysi.R   # per-lab starting points, emit dat contract
  format_wqx.R  apply_qaqc_flags.R  generate_cdx_export.R
  fetch_raw_data.R  archive_year.R                          # restore / bundle a year
  (report helpers carried from kenai-river-wqx)
reference/                # master_reg_limits.xlsx, baseline_sites.csv, WQX matching tables, dat_contract.md
data/                     # raw lab files for the working year (contents gitignored)
data/{year}.sha256        # committed checksum per archived year
output/                   # dat, lab_qc_assessed.csv, assessed_results.csv, CDX files, rendered report
project_year/             # legacy per-year QMDs, read-only until retired
```

Past years live only as GitHub Release assets (see decisions 10 to 12). History already in
the qaqc repo (~460 MB) is left as is.

## The seam: the `dat` contract

Ingestion ends at a standardized `dat` data frame (`reference/dat_contract.md`). Downstream
steps use only that contract: WQX formatting, QA/QC flags, the 42-question checklist, CDX
export, and the report. The QA/QC stage writes `output/assessed_results.csv`; the report
reads only that file (schema in decision 9).

## Phases

Each phase ends with a pause for user review. There is no cross-year regression baseline;
2025 is verified by explaining every difference from the existing 2025 outputs
(`2025.qmd`, `2025_preliminary_summary.qmd`), including known defects (surrogates in `dat`,
field blank coding, `mg/l` case).

### Phase A: Contract, layout, and archive mechanics
1. Finish `reference/dat_contract.md` (`dat` and `lab_qc_dat`).
2. Create the layout above (empty scaffolds); gitignore `data/` contents and `output/`
   as agreed; move shared reference data into `reference/`.
3. Reconcile the three function copies (`other/misc/qaqc_repo_transition/functions/`)
   into `functions/`.
4. Write `fetch_raw_data(year)` (download release zip, verify `data/{year}.sha256`) and
   `archive_year(year)` (build the year bundle, write checksum), and document the
   new-year and archive checklists in the README.

### Phase B: 2025 ingestion as functions
1. Extract SGS, FC, TSS, YSI ingestion from `2025.qmd` into `functions/ingest_*.R`
   (season argument; 2025 quirks in `config.yml` or edited directly).
2. Route `(surr)` analytes to `lab_qc_dat`; keep trip blanks in field data; drop `matrix`
   and `result_detection_condition`; add the `mg/L` guard.
3. Add a contract-check function that stops with a clear message on any violation.
4. Verify: `dat` row counts and values vs `2025.qmd`, with each known difference explained.

### Phase C: QA/QC and CDX export
1. Build `qaqc.qmd` Parts B-D on the contract output; add field blank coding (Activity
   Type `Quality Control Sample-Field Blank`, suffix `FB`) and the surrogate check in
   Question 20.
2. Write `output/assessed_results.csv` and `output/lab_qc_assessed.csv`.
3. Verify against the 2025 CDX export files and checklist results; explain differences.

### Phase D: Narrative report
1. Generalize `2025_preliminary_summary.qmd` into `prelim_report.qmd`: delete its
   ~330-line setup-chunk re-ingestion (about lines 200-530) and read
   `assessed_results.csv` instead.
2. Parameterize year, dates, captions, and photo references; stop if the assessed file
   is missing or stale.
3. Verify 2025 numbers and tables against the existing summary.

### Phase E: Archive 2025 and set up for 2024
1. Run the archive checklist: build the 2025 bundle, upload release `raw-data-2025`,
   commit its checksum file.
2. Promotion review: diff the 2025 working files against the template and decide what
   goes back into the template (assistant drafts the diff).
3. Reset the working tree for 2024; retire the 2025 and 2023 legacy `project_year` files
   as each year is superseded.
4. Update both repos' `AGENTS.md` and README files for the single-year template design.

Later years (2024, 2023, ...) repeat: start year, customize ingestion, QA/QC, report,
archive, promotion review.

## Decisions log (2026-10-28)

1. (Superseded by 8i/decision 11.) Earlier: 2021 as regression baseline. Dropped because
   per-year customization means one year cannot validate another. 2021 gets no special role;
   it is reached in the backwards sequence like any year.
2. Surrogates are ingested as designated lab QC samples (`lab_qc_dat`), not field data.
3. Field blanks only exist from about late 2022 or 2023 onward; field-blank handling must
   be optional per year.
4. `dat` drops `matrix` and `result_detection_condition` (24 columns). `dissolved` stays in
   both `dat` and `lab_qc_dat`.
5. Field blank coding: Activity Type `Quality Control Sample-Field Blank`, Activity ID suffix
   `FB`. Trip blank suffix `Blank` is unchanged. HMW charts exclude Activity Types containing
   "quality" (station summary counts and WQP downloads are unfiltered).
6. `lab_qc_dat`: all 14 SGS lab QC codes plus `(surr)` analytes from any sample type; keeps
   recovery/RPD columns and the lab-supplied limits (shipped in the EDD, used by Question
   20). Question 20 extends to surrogates. Never flows into the WQX export. Profile and
   proposed columns: `reference/dat_contract.md`.
7. Lab format: one ingest function per lab (SGS, FC, TSS, YSI) taking a season argument.
   The `adapters/{year}.R` idea is dropped (see 11).
8. Narrative report: separate `prelim_report.qmd`, rendered after `qaqc.qmd`, reading only
   `output/assessed_results.csv`, `config.yml`, and reference data (never raw lab files).
   Failed data is excluded through flag columns; the report applies no filters of its own.
9. `assessed_results.csv`: the 24 `dat` columns under ingest names plus `result_id` (stable
   row key within the year), `qaqc_status` (Accepted/Rejected/Flagged), `qaqc_flags`
   (semicolon-separated checklist questions, empty string if none), `qaqc_note` (NA if none),
   and `use_in_report` (derived from `qaqc_status`). One row per result; lab QC rows are
   excluded and written to `output/lab_qc_assessed.csv`. WQX renaming and Activity IDs stay
   in the export step. The report and the CDX export both derive accept/reject from
   `qaqc_status`.
10. Raw data hosting: the repo holds one year's raw data at a time (`data/` contents
    gitignored). Archived years are GitHub Releases, one zip per year (`raw-data-{year}`) on
    the qaqc repo. The repo is public and all lab files (PDFs, COC scans, .mdb, data sheets)
    may be published, so zips hold everything the labs sent and fetching needs no login. A new
    year's files are placed by hand in `data/`; an archived year is restored with
    `fetch_raw_data(year)`. Each archived year has a committed checksum file
    (`data/{year}.sha256`) verified on fetch.
11. Year bundle: each release zip holds raw lab files, `config.yml`, the edited pipeline
    files for that year, `output/` (including QA/QC flags), and the rendered report. The
    repo holds only the working year. Existing history is left as is (no rewrite). Years are
    developed one at a time, 2025 first and then backwards; no cross-year regression check.
    Per-year customization is done by editing the working copy directly (the repo is a
    template); those edited files are archived in the bundle.
12. Promoting fixes: the archive checklist includes a review step that diffs the year's
    working files against the template; you decide what to copy back, and the assistant
    drafts the diff.

## Open items

- Confirmed 2026-10-28: hard-stop `mg/L` guard at ingest (same `stopifnot()` as the report
  repo) and the `session_log.md` sync Action stays as is.
- Checked 2026-10-28: 2025 raw files total 31 MB (largest file 15.6 MB), far under the 2 GB
  per-asset release limit.

## Out of scope

- Changes to how the report repo reads WQP or renders the book.
- Performing CDX submissions for any year.
- Rewriting qaqc repo git history.
13. Quarto setup (2026-10-28): the qaqc repo's Quarto book config is replaced by a plain
    project (`_quarto.yml`, type default). `qaqc.qmd` and `prelim_report.qmd` render on their
    own; legacy `project_year/*.qmd` remain read-only files outside the build until retired.

14. Working-year `output/` (2026-10-28): committed while working. The archive step builds the
    bundle, then removes `output/` from the tree before the next year starts.

## Phase A progress

- Done (qaqc repo, uncommitted): `reference/dat_contract.md`; `config.yml` (2025, paths relative
  to `data/`, matching the delivered folder names); `data/.gitignore`; plain `_quarto.yml`;
  `other/input/wqx_templates` moved to `reference/wqx_templates` (git mv); the three function
  files COPIED to `functions/` (the `other/misc/qaqc_repo_transition/functions/` originals
  remain until reconciled); 2025 raw files moved to `data/` and untracked (`git rm --cached`;
  still in git history); `project_year/2025.qmd` paths updated. Check: Part A of 2025.qmd
  (up to the `format_wqx.R` source) reproduces `dat` = 883 rows, SGS 567 / SWWTP 94 / YSI 222.
- Not yet done: `other/output/` is not yet moved to `output/` (2025 and 2023 files are mixed
  there); `reference/wqx_templates` still holds year-specific files (`2025_data_flag_decisions.csv`,
  `AWQMS_KWF_Baseline_2025.xlsx`, `trip_blank_crews_2025.csv`) that belong in the year bundle;
  `master_reg_limits.xlsx` and `baseline_sites.csv` live only in the report repo and are needed
  by `prelim_report.qmd` (Phase D); reconcile the function copies; write `fetch_raw_data()`
  and `archive_year()`; README checklists.
