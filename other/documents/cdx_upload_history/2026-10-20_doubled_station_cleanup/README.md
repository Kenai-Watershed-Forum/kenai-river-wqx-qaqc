# Doubled station ID cleanup (2021 KWF data)

Created 2026-10-20 by Benjamin Meyer (hydrology@kenaiwatershed.org), with Posit Assistant.

## What happened

The corrected 2021 dataset was imported to WQX Web on 2026-10-14 (Uid 9481, all 830 rows, no validation errors) and submitted through CDX. By 2026-10-20 it was visible on the Water Quality Portal (WQP), and a row-level comparison against `other/output/wqx_formatted/results_activities.csv` matched all 830 rows one-to-one with no differences in values, detection limits, or lab name.

However, the 2021 results are stored under the wrong stations:

- The uploaded `Monitoring Location ID` (results file) and `MonitoringLocationIdentifier` (station file) contained the org prefix, for example `KENAI_WQX-10000002`.
- WQX Web prepends the org prefix itself on import, so WQP now holds 22 new stations named like `KENAI_WQX-KENAI_WQX-10000002`.
- Those 22 doubled stations hold all 830 2021 results. The 22 original stations (`KENAI_WQX-100000xx`) have no 2021 data.
- Effect: the 2021 data is disconnected from each site's time series (2000-2013 and later years) and from any join on `MonitoringLocationIdentifier`.

This is the same rule already documented for Activity IDs in `chapters/cdx_upload.qmd`: never include the `KENAI_WQX-` prefix in any ID column. Activity IDs were already bare, so they were not affected.

Root cause: `functions/appendix_a_scripts/generate_cdx_export.R` copied WQP's `MonitoringLocationIdentifier` unchanged. That script has been fixed (prefix stripped in both files) and the upload files regenerated; only the ID columns changed (830 cells in `results_activities.csv`, 22 in `station.csv`).

## Files in this folder

| File | Purpose |
|----|----|
| `resultphyschem_DELETE_doubled_stations_2021.csv` | DELETE file: 830 Activity IDs (prefix stripped, CRLF, one header row `ActivityIdentifier`), the same format as earlier DELETE files. |
| `monitoringlocation_DELETE_doubled_stations.csv` | Station DELETE file: 22 IDs in WQX-stored form (`KENAI_WQX-100000xx`, one prefix). Checked against the original stations in the script. Use in step 3, after step 1. |
| `doubled_stations_to_remove.csv` | The 22 doubled station IDs as shown on WQP (reference only, do not import). |
| `make_delete_file.R` | Reproduces both files from the live WQP cache. Includes checks (830 IDs, no prefix). |

The delete file was built from the WQP live cache (`other/input/WQX_downloads/wqp_live/narrowresult.csv`, refreshed 2026-10-20). Activity IDs on the doubled stations are not shared with any other station, so the delete only affects the 830 misplaced results.

## Cleanup plan

Deletion in WQX is permanent. Keep local copies of all submission files. Nothing has been uploaded as of this writing.

1. **Delete the 830 results.** WQX Web: Import & Submit, Import a batch of IDs for records to delete from WQX, Import a file of Activity IDs to be deleted (labelled "Activity Group IDs to Delete" in the UI). Use `resultphyschem_DELETE_doubled_stations_2021.csv`. Procedure details: `chapters/cdx_upload.qmd`, section "Deleting Previously Submitted Data".
2. **Verify** on WQP (rerun `functions/refresh_wqp_data.R`): 0 rows for 2021 on the doubled stations.
3. **Delete the 22 doubled stations.** WQX Web has an import type for this: Import & Submit, then the "To Delete Data in WQX (One Column: Primary ID)" list, then "Import a File of Monitoring Location IDs to be deleted" (confirmed present 2026-10-20). Do this only after step 1, since WQX may refuse to delete stations that still have results. **Caution:** the file must list the IDs as WQX stores them, which is the WQP ID minus one prefix (for example `KENAI_WQX-10000002`, not `10000002`). A bare `10000002` would target the 22 original, correct stations. Check the file before importing. If deletion is refused, inactivate the stations on the Station screen and record that here.
4. **Re-upload** the corrected `station.csv`, then `results_activities.csv` (bare IDs), from `other/output/wqx_formatted/`. Submit to CDX.
5. **Re-verify** on WQP: 830 rows for 2021 across the 22 original single-prefix stations, none on doubled stations. Rerun `functions/refresh_wqp_data.R`.

Open question to check at step 4: it is expected, but not yet tested, that WQX Web matches the bare station IDs to the existing 22 original stations rather than creating new ones.

## Log

| Date | Step | Result |
|----|----|----|
| 2026-10-20 | Delete file generated | 830 IDs, 22 stations listed. |
| 2026-10-20 | Step 1: activity delete imported and submitted | WQX Web dataset 117563 ("Activity IDs to Delete", file `resultphyschem_DELETE_doubled_stations_2021.csv`). Import 2:23 to 2:26 PM: 830 records, 830 valid, 0 invalid (`Import Log.xlsx` in this folder). Export and submission to CDX 2:27 to 2:29 PM, status "Completed at CDX". WQX states the data may take up to four days to be published on WQP. |
| 2026-10-20 | Step 3: station delete imported and submitted | WQX Web dataset 117564 ("Monitoring Location IDs to Delete", file `monitoringlocation_DELETE_doubled_stations.csv`). Import 2:45 PM, export and submission to CDX 2:46 PM, status "Completed at CDX", no refusal. Import log (`Import Log - station delete.xlsx`): 22 records, 22 valid, 0 invalid. This succeeded before WQP showed the activity delete, so WQX's own database had already applied step 1. |
| 2026-10-20 | WQP check after submission | Still 830 rows on the doubled stations (expected, publication lag). Recheck from 2026-10-24 or earlier if it clears. |
| 2026-10-25 | WQP check | Deletes published: 15,605 result rows (2000-2013), 0 doubled-prefix rows. |
| 2026-10-25 | Step 4: re-upload | `project.csv` (117639) and `station.csv` (117640) with "existing data only"; `results_activities.csv` with "new data only". First results attempt (117641) failed: "More columns than expected" at Row 2 because the older `KWF_Results_Baseline_Template` configuration was selected. Re-imported with `KWF_Results_Baseline_Template_v2` (Uid 9481) as dataset 117643. All three "Completed at CDX". |
| 2026-10-25 | Step 5: re-verify | Pending WQP publication. Expect 830 rows for 2021 on the 22 original stations. |
