# kenai-river-wqx-qaqc

QA/QC, WQX/CDX export, and preliminary report for the Kenai River Baseline Water Quality Monitoring program (Kenai Watershed Forum). The finished, multi-year book lives in [kenai-river-wqx](https://github.com/Kenai-Watershed-Forum/kenai-river-wqx) and reads from EPA's Water Quality Portal.

This repo holds **one working year at a time**: that year's raw lab files, configuration, outputs and report, plus the shared code and reference data. A finished year is archived as a GitHub Release ("year bundle") and removed from the working tree.

Migration status: the pipeline is being moved from per-year QMDs (`project_year/`) to a single `qaqc.qmd` driven by `config.yml`. Until that is finished, the working year's pipeline is `project_year/{year}.qmd`. Plan and phases: see the report repo's `.posit/assistant/plans/` (not tracked in git).

## Layout

| Path | Contents |
|----|----|
| `config.yml` | Working-year settings: year, sample dates, lab file paths (relative to `data/`). Edit per year. |
| `data/` | Raw lab files for the working year. Contents are gitignored; only `{year}.sha256` files are committed. |
| `functions/` | Shared code, including `fetch_raw_data.R` and `archive_year.R`. |
| `reference/` | Shared reference data (`dat_contract.md`, `wqx_templates/`). |
| `year_files/` | Files specific to the working year (flag decisions, AWQMS template, trip blank crews). Bundled at archive time. |
| `output/` | Working-year outputs, committed while the year is in progress. |
| `project_year/` | Legacy per-year QMDs, read-only once the year is archived. |

## New-year checklist

1. Confirm the previous year is archived (see below), or that you intend to work two years side by side (not supported; archive first).
2. Place the labs' files in `data/` (spring and summer folders, YSI workbook). Do not commit them; `data/.gitignore` already excludes them.
3. Edit `config.yml`: `year`, `sampling` dates, and every path under `files:`. Lab file names and sheet names change between deliveries, so check each one.
4. Create `year_files/` inputs: `{year}_data_flag_decisions.csv`, `AWQMS_KWF_Baseline_{year}.xlsx`, `trip_blank_crews_{year}.csv`. Copy from the prior year's bundle (`fetch_raw_data(prior_year, what = "all")`) and edit.
5. Run the pipeline. Fix intake problems in the working copy of the code; do not work around them downstream.
6. Check `reference/dat_contract.md` invariants. The pipeline stops on a units-case mismatch (`mg/L`).
7. Render the report and review outputs in `output/`.

## Archive checklist

Run from the working copy that still holds the year.

1. Confirm `config.yml` has `year: {year}` and that `output/` is final and committed.
2. Build the bundle:
   ```r
   source("functions/archive_year.R")
   archive_year(2025)
   ```
   This writes `archive_staging/raw-data-{year}.zip` (gitignored) and `data/{year}.sha256`. It deletes nothing.
3. Upload: GitHub, Releases, Draft a new release. Tag `raw-data-{year}`, attach the zip, publish. The repo is public, so everything the labs sent is publicly downloadable.
4. Promotion review: diff the year's edited code (`project_year/{year}.qmd` or `qaqc.qmd`, `functions/`) against the template and decide which fixes belong in the shared code. Do this before step 5 deletes the year's files. Ask the assistant to draft the diff; you decide what is copied back.
5. Verify and clear the working year:
   ```r
   remove_archived_year(2025)
   ```
   This downloads the published asset, checks it against `data/{year}.sha256`, and only then deletes the year's raw files, `year_files/`, and `output/` (after asking for confirmation).
6. Commit: `data/{year}.sha256`, any promoted code changes, and the removal of the year's working files.
7. Update both repos' `AGENTS.md` and, if the year's data are now on WQP, run `functions/refresh_wqp_data.R` in the report repo.

## Restoring an archived year

```r
source("functions/fetch_raw_data.R")
fetch_raw_data(2025)                 # raw lab files into data/
fetch_raw_data(2025, what = "all")   # also config, code, outputs and report into archive_restore/2025/
```

`fetch_raw_data()` downloads `raw-data-{year}.zip` from the release, verifies it against the committed `data/{year}.sha256`, and refuses to unpack on a mismatch. It stops if `data/` already holds files.

## Bundle contents

| In zip | Contents |
|----|----|
| `raw/` | Everything that was in `data/`, as delivered |
| `year/` | `config.yml`, the pipeline QMD(s) that exist, `year_files/`, `output/` including the rendered report |
| `MANIFEST.csv` | Path, size and SHA-256 of every bundled file |

Requires the `digest`, `zip` and `yaml` packages.
