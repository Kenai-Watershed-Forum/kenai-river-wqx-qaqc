# Lab report PDF extraction

Transcribes analytical lab-report PDFs into the KWF/EPA results template (long format, one row per analyte observation). Use it only for events that have no electronic data deliverable. Nothing here is year-specific.

## Two stages

| Stage | File | What it does |
|---|---|---|
| 1 | `extract_ars_pdf.R` | `extract_ars_pdf(path)` and `extract_ars_dir(dir)` read the report text layer and return what the PDF states: `results`, `method_blanks`, `surrogates`, `qc`, `accounting`, `by_sample`, `case_narrative`, `sample_receipt`. Stops on any analyte, QC or receipt line it cannot parse. |
| 2 | `map_ars_to_template.R` | `map_ars_to_template(extract)` returns `list(template, lab_qc, case_narrative, sample_receipt, checks)`. Project constants and rules are in `template_constants.yml`; column order is in `template_columns.csv`. |

The ARS Aleut and Analytica Group reports (2014 to 2018) share one layout family. Wording variants are handled in Stage 1; a new variant is added once and then covers every season that uses it.

## Typical run

```r
source("functions/pdf_extract/extract_ars_pdf.R")
source("functions/pdf_extract/map_ars_to_template.R")
source("functions/pdf_extract/write_template_workbook.R")
source("functions/pdf_extract/check_template_ready.R")

ex <- extract_ars_dir("path/to/_docs")
mapped <- map_ars_to_template(ex)
write_template_workbook(mapped, ex, "path/to/event_v2.xlsx")
check_template_ready("path/to/event_v2.xlsx")
```

Check `ex$accounting` (cover-page samples versus samples with results) before trusting an event.

## Conventions (template v2)

- Non-detect: blank Result Value, qualifier `U`, Result Detection Condition `Not Detected`; `J` gives `Present Below Quantification Limit`.
- Time zone `AKST` on every row.
- Trip blanks: Activity Type `Quality Control Sample-Trip Blank`, Sample Condition `Trip Blank`.
- Surrogates and other lab QC go to the `lab_qc` sheet only and are never uploaded to CDX.
- Receipt date and time are columns on the template sheet; receipt temperature is on the `sample_receipt` sheet (QA/QC only).
- Monitoring Location ID, Activity ID, Result Sample Fraction and Result Status ID are left blank here and filled in a later step.

## Hand-entered workbooks (2016)

`correct_hand_entered.R` writes a `_corrected.xlsx` beside a hand-entered workbook (original untouched, changes shaded and logged). `upgrade_to_v2.R` then writes `_v2.xlsx` from the corrected file. These two exist for the 2016 workbooks, which carry site and Activity ID columns worth keeping. The 2014 and 2015 workbooks contain mistyped lab sample numbers and rows in the wrong place, so those events are built from the PDFs directly.

## Tests

`tests/test_pdf_extract_v2.R`, `tests/test_pdf_extract_analytica.R`, `tests/test_check_template_ready.R`.
