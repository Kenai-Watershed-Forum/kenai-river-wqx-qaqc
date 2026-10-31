# Per-Year Repo Architecture Migration — Plan

## Context / findings from intake

- **Report repo (`kenai-river-wqx`)**: `.git` is 1.7 GB, almost entirely from dozens of
  historical commits of `docs/appendix_a.html` (~31 MB each, re-rendered and re-committed
  repeatedly). `other/input/WQX_downloads` (545 MB, gitignored) is not the git-size problem.
- **qaqc repo (`kenai-river-wqx-qaqc`, local clone at `D:\GitHub\kenai-river-wqx-qaqc`)**:
  in sync with origin. Already contains real, working progress beyond what this report
  repo's memory reflected:
  - `project_year/{2014-2025}.qmd` — one QMD per year already scaffolded
  - `2025.qmd` — a largely complete pipeline (Part A ingestion for SGS/FC/TSS/YSI, Part B
    WQX formatting, Part C 42-question QA/QC checklist, Part D CDX export)
  - `2023.qmd` — skeleton rebuilt to the 2025 template, chunks `eval: false`, not yet ported
  - `.git` is 201 MB — not currently a size problem
- **`other/qaqc_repo_transition/`** (in this report repo) is a stale, superseded duplicate.
  Diffed its `functions/*.R` and `templates/pipeline_template.qmd` against the live qaqc
  repo's `other/misc/qaqc_repo_transition/` copies: the live copies are strictly newer
  (bug fixes: `na = ""` CSV export, `Monitoring Location Name` field, join robustness,
  `colClasses` fix). The three `reference/2021_ingest_*.R` files are byte-identical.
  **No unique content would be lost by deleting it.**
- **Preliminary report**: `other/documents/preliminary_report_2025/2025_preliminary_summary.qmd`
  (989 lines) already exists in this report repo as a standalone per-year summary document,
  separate from the integrated multi-year book. This is the natural basis for the "preliminary
  report" piece each new year-repo should produce.

## Confirmed decisions

1. **Architecture**: one GitHub repo per data year (not one qaqc repo with many `{year}.qmd`
   files). Each year-repo will, from a shared template: (a) ingest + QA/QC the year's data,
   (b) prepare CDX upload files, (c) use the QA/QC'd data to compose a preliminary annual
   report — all in the same repo.
2. **`other/qaqc_repo_transition/`**: delete from this report repo (confirmed no unique content).
3. **Git history rewrite**: proceed in **both** repos (user is sole contributor, re-cloning
   is not a concern). Report repo is the priority (1.7 GB); qaqc repo bloat is minor (201 MB)
   but will be addressed too.
4. **Per-year repo data**: raw/intermediate data for a given year does **not** need to be
   gitignored in that year's repo — one year of lab data is a manageable size to commit.
5. **GitHub access**: no `gh` CLI auth in this environment; user has local clones instead
   (`D:\GitHub\kenai-river-wqx-qaqc` already exists and is up to date). New repos/clones will
   be handled via local git + the user doing GitHub-side repo creation (or via git remote
   push to an already-created empty repo) rather than `gh`.
6. **Template repo**: a new repo, **`kenai-river-baseline-wqx-template`**, under
   `Kenai-Watershed-Forum`. `kenai-river-wqx-qaqc` is **archived** (not repurposed) once
   migration is complete, kept as a read-only historical artifact.
7. **Per-year repo naming**: **`kenai-river-wqx-{year}`** (e.g. `kenai-river-wqx-2025`).
8. **Legacy years (2014-2022 placeholders, 2023 partial)**: do **not** pre-create repos for
   these now. A real per-year repo is created from the template only when work actually
   begins on that year. Until then, the existing placeholder/partial QMDs simply remain inside
   `kenai-river-wqx-qaqc`, which becomes read-only reference material once archived.
9. **Shared reference data** (`master_reg_limits.xlsx`, `baseline_sites.csv`, WQX matching
   tables): **per-repo copy**. Each new year-repo gets a fresh snapshot copied in at creation
   time from the template; no shared external source. Accept some duplication/drift risk since
   these change rarely.
10. **Migration order**: **2025 first.** It has the most complete existing pipeline and becomes
    the end-to-end validation case for the template, before rolling backward to older years.
11. **Preliminary report**: generalize `other/documents/preliminary_report_2025/2025_preliminary_summary.qmd`
    (from this report repo) into a year-parameterized template component, rather than writing a
    new report format from scratch.

## Execution sequencing

Work proceeds one phase at a time, stepwise within each phase, with an explicit check-in
between steps (not a whole phase unattended). At the end of each phase, pause and prompt
the user before starting the next phase.

**Status as of 2026-10-14: Phase 0 and Phase 1 complete (both pushed to origin). Phase 2 (build the template repo) is next — start there in a fresh conversation.**

### Phase 0 — Housekeeping — COMPLETE (local commits only, not pushed)

**Step 0.1 — Delete `other/qaqc_repo_transition/` from the report repo.** DONE. Committed locally as `4f6247e`. Not yet pushed to origin.
- Confirmed superseded (see diff findings above).
- Action: remove the directory, commit with a message explaining why (superseded by live
  work in `kenai-river-wqx-qaqc`).
- Not pushed until the user reviews the commit.

**Step 0.2 — Correct this report repo's `AGENTS.md`.** DONE.
- Committed locally as `4f9429a` — "Correct AGENTS.md: qaqc repo is further along than previously recorded". Not yet pushed to origin. Added an "Architecture Change In Progress" section near the top of `AGENTS.md` pointing back to this plan file.
- Scope limited to *factual corrections*, not the future architecture specifics:
  - Update the description of `kenai-river-wqx-qaqc`'s actual current state (per-year QMDs
    already exist for 2014-2025; 2025.qmd is a largely complete working pipeline; 2023.qmd
    is a partially-ported skeleton) so Task 18/19 statuses reflect reality rather than the
    stale "Pending" status.
  - Remove the reference to `other/qaqc_repo_transition/` once it's deleted (Step 0.1).
  - Add a short "Architecture change in progress" note pointing to this plan file, so a future
    session picks up the context without needing to re-derive the resolved decisions above.
- Depends on Step 0.1 being done first.

### Phase 1 — Git history cleanup (both repos) — COMPLETE

**Report repo (`kenai-river-wqx`): DONE.**
1. Identified strip scope via `git rev-list --objects --all` + size sort: `docs/**` (all of it,
   3.44 GB across history, dominated by 59 versions of `docs/appendix_a.html`), `*_cache/`
   (1.84 GB, knitr/rmarkdown caches), `*_files/` outside `docs/` (0.14 GB), and stray root
   `appendix_a.html`/`README.html` (31.6 MB). Total stripped: 5.07 GB of 6.23 GB raw history.
2. Replacement policy: switched to `quarto publish gh-pages` (pushes rendered site to a
   dedicated `gh-pages` branch). `docs/` is now gitignored on `main` and never committed there.
   GitHub Pages source switched from "main /docs" to "gh-pages /(root)" — confirmed live and
   rendering correctly.
3. Backed up full pre-rewrite history as a git bundle (scratch dir) before rewriting.
4. Ran `git filter-repo` (installed via `pip install git-filter-repo`) with `--invert-paths`
   and a `--paths-from-file` combining `literal:docs`, `literal:appendix_a.html`,
   `literal:README.html`, `regex:(^|/)[^/]+_cache/`, `regex:(^|/)[^/]+_files/`. Result: `.git`
   shrank from 1.48 GB (pack) to 224 MB. Force-pushed rewritten `main` to `origin`.
   `gh-pages` branch initialized and first publish pushed successfully.
   README updated to document the new `quarto publish gh-pages` workflow.

**qaqc repo (`kenai-river-wqx-qaqc`): history rewrite explicitly skipped by user decision.**
- Same bloat pattern found (`docs/2023.html`: 225 MB across 10 versions; `docs/` overall 221 MB,
  `*_cache/` 38 MB, out of ~460 MB total raw history — much smaller than the report repo to
  begin with).
- Decision: not worth rewriting given the repo's small absolute size and planned archival once
  the per-year-repo migration completes (Phase 3). Left as-is, no filter-repo run, no gh-pages
  switch.
- Unrelated bug fixed in passing: `.gitignore` had literal unresolved git merge-conflict markers
  committed (`<<<<<<< HEAD` / `=======` / `>>>>>>> 7830d00...`), silently breaking the ignore
  rules below that point. Fixed and pushed (commit `2976400`).

### Phase 2 — Build `kenai-river-baseline-wqx-template` and validate with 2025
1. Create the new `kenai-river-baseline-wqx-template` repo (GitHub side: user creates the
   empty repo; local side: scaffold and push).
2. Generalize `kenai-river-wqx-qaqc`'s `2025.qmd` (Parts A-D) into the template's pipeline,
   parameterized via the existing `cfg` pattern (year, paths, sample dates).
3. Generalize `2025_preliminary_summary.qmd` (from this report repo) into a year-parameterized
   preliminary-report component of the template.
4. Carry forward the stable shared functions using the **live qaqc repo's versions** as the
   source of truth (not the deleted `other/qaqc_repo_transition/` copies): `format_wqx.R`,
   `apply_qaqc_flags.R`, `generate_cdx_export.R`, plus report-repo functions needed for the
   preliminary report (`static_boxplot_function.R`, `threshold_table.R`, `render_plots.R`,
   `table_download.R`).
5. Include a fresh per-repo copy of shared reference data (`master_reg_limits.xlsx`,
   `baseline_sites.csv`, WQX matching tables) in the template.
6. Create `kenai-river-wqx-2025` from the template and reconstruct 2025 end-to-end
   (ingestion → QA/QC → CDX export → preliminary report render), comparing output against
   the existing `2025.qmd` / `2025_preliminary_summary.qmd` results to confirm no regressions.

### Phase 3 — Roll out and retire old structure
1. Once `kenai-river-wqx-2025` is validated, create additional per-year repos from the
   template only as work begins on each year (no pre-emptive creation).
2. Archive `kenai-river-wqx-qaqc` once it's no longer needed as an active workspace.
3. Update this report repo's `AGENTS.md` / companion context files to describe the new
   multi-repo-per-year architecture and drop references to the old single-qaqc-repo design.

## Explicitly out of scope for this plan

- Any change to how the report repo (`kenai-river-wqx`) reads from WQP or renders the
  integrated multi-year book — only its git history size is being addressed here.
- Actually performing CDX submissions for any year — this plan only covers the tooling/repo
  architecture, not re-running or re-submitting QA/QC for specific years.
