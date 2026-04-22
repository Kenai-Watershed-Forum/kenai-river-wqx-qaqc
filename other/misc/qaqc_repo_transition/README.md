# Kenai River Baseline WQX — Annual QA/QC Pipeline Repo

This repository contains the per-year QA/QC pipeline for the Kenai Watershed Forum (KWF)
Kenai River Baseline Water Quality Monitoring project. Each data year's complete ingestion,
QA/QC evaluation, flagging, and CDX export workflow is contained in a single Quarto document.

**Related repos:**
- Report repo (2021 worked example + public report): https://github.com/Kenai-Watershed-Forum/kenai-river-wqx
- This repo (annual pipeline, canonical home): https://github.com/Kenai-Watershed-Forum/kenai-river-wqx-qaqc
- Public output: https://kenai-watershed-forum.github.io/kenai-river-wqx-qaqc/

---

## Directory Structure

```
templates/
  pipeline_template.qmd   # Canonical template — copy and rename for each new year
functions/
  format_wqx.R            # Stable WQX column formatting (do not modify year-to-year)
  apply_qaqc_flags.R      # Stable flag join + write (do not modify year-to-year)
  generate_cdx_export.R   # Stable CDX export writer (do not modify year-to-year)
reference/
  2021_ingest_sgs_als.R   # 2021 worked example: SGS/ALS ingestion
  2021_ingest_fc.R        # 2021 worked example: Fecal Coliform ingestion
  2021_ingest_tss.R       # 2021 worked example: TSS ingestion
{year}.qmd                # Per-year pipeline (one file per data year)
AGENTS.md                 # Posit Assistant memory file
```

---

## Starting a New Data Year

1. Copy `templates/pipeline_template.qmd` and rename it `{year}.qmd` (e.g., `2023.qmd`).
2. Open the new file and update the **## Year Configuration** block at the top:
   - Set `year`, sampling dates, FC/TSS file paths, and FC lab selectors.
   - The `cfg` list will be built automatically from those variables.
3. Work through **Part A: Data Ingestion**, adapting the skeleton code to match the current
   year's lab EDD formats. Use the scripts in `reference/` as worked examples from 2021.
4. Render the document; Parts B, C, and D run from the stable `functions/` scripts.
5. Complete manual entries in Part C (QA/QC checklist questions marked `# MANUAL:`).
6. Run Part D to generate CDX upload files in `other/output/wqx_formatted/`.

---

## Architecture Notes

- **Part A ingest code is inlined** in each year's QMD (not sourced). This keeps year-specific
  format quirks visible and explicitly marked for adaptation. The `reference/` scripts are
  provided for copy-and-adapt, not for sourcing.
- **Parts B and D source stable scripts** from `functions/`. These should not need changes
  year to year. If a change is needed, document it thoroughly.
- **All paths flow through `cfg`**, a list constructed in the Year Configuration block.
  The stable scripts read from `cfg` rather than bare globals, making dependencies explicit.
- **Raw data rule:** Never modify files in `other/input/`. All transformations happen in code.

---

## Key External Links

- EPA WQX CDX submission: https://cdx.epa.gov/
- Water Quality Portal (public data): https://www.waterqualitydata.us/ (org: `KENAI_WQX`)
- WQX web template files: https://www.epa.gov/waterdata/water-quality-exchange-web-template-files
- ADEC Integrated Report / CALM: https://dec.alaska.gov/water/water-quality/integrated-report/
- Project home: https://www.kenaiwatershed.org/kenai-river-baseline-water-quality-monitoring/
