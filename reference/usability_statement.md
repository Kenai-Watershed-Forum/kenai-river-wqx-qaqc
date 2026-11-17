## Data usability statement

All results from the monitoring year are submitted to EPA WQX and published through the Water Quality Portal, including results that failed a QA/QC check. Each result carries a Result Status of either Accepted or Rejected.

A result is Rejected when it fails one of three checks:

- **Laboratory QC.** A failing laboratory control sample, calibration verification, surrogate or similar check rejects the results in the same analytical batch for the same analyte and method. A failing matrix spike or matrix spike duplicate rejects only its parent sample, because those failures reflect the sample matrix and not the batch.
- **Blanks.** A laboratory blank with any detection rejects detected results in the same batch that are below a set multiple of the blank result. The multiple is a Kenai Watershed Forum project convention, stated in `config.yml`. The QAPP does not specify one.
- **Field duplicates.** A duplicate pair that fails the relative percent difference criterion in the QAPP rejects both results of the pair.

Rejected results are flagged with the qualifier `QCF` in Result Qualifier, and the reason is recorded in Result Comment. Rejected results are not removed from the dataset. Users who need only data that passed QA/QC should filter on Result Status.

Field blank and trip blank results are reported and listed in the blank check of the QA/QC report. They do not currently cause other results to be Rejected.

Low level quantitation check (LLQC) samples are not evaluated, because acceptance limits are not documented. Qualifier codes follow QAPP v3 Table 7; definitions are in `reference/qualifier_definitions.csv`. Counts of Rejected results, the analytes affected and the checks that failed are given in the QA/QC report for the year.
