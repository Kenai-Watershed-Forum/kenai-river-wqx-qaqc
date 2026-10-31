# Sample Fraction Name Corrections — Handoff to `kenai-river-wqx-qaqc`

**Prepared:** April 2026\
**From:** `kenai-river-wqx` repo (2021 baseline report)\
**To:** `kenai-river-wqx-qaqc` repo (annual QA/QC pipeline)

------------------------------------------------------------------------

## Background

EPA's How's My Waterway (HMW) treats records with different `ResultSampleFractionText` values as separate data series, presenting a dropdown menu so users can view one fraction at a time. KWF's dissolved metals data is currently split across two fraction names in WQP — `"Dissolved"` for 2000–2013 and `"Filtered, field"` for 2021 — so HMW cannot display the full time series at once.

Additionally, starting in spring 2023, KWF changed its dissolved metals protocol from **field filtration** to **lab filtration** (both using a 0.45 µm filter). This means future data would require `"Filtered, lab"` if fraction names were intended to describe the filtration method — which would create a third split in HMW.

The resolution is to recognize that `ResultSampleFractionText` should describe the **analytical fraction measured** (dissolved vs. total), not the specific filtration procedure used to obtain it. The method description and QAPP document the filtration procedure. Using `"Dissolved"` for all dissolved metals results, regardless of whether filtration was done in the field or lab, keeps the full time series unified in HMW.

------------------------------------------------------------------------

## Canonical Sample Fraction Scheme

The following values should be used for **all data years** in all CDX submissions. These are valid WQX domain values.

| Parameter type | Canonical `ResultSampleFractionText` | Notes |
|----|----|----|
| Dissolved metals (any filtration method, any analytical method) | `Dissolved` | Describes the analytical fraction, not the filtration procedure. Consistent across 2000–2013 (field-filtered 200.8), 2021 (field-filtered 200.8), and 2023+ (lab-filtered 200.8). |
| Total metals (unfiltered, any analytical method) | `Unfiltered` | Consistent for 200.7 (pre-2023) and 200.8 unfiltered (2023+). |
| Nutrients (nitrate/nitrite, phosphorus) | `Total` | Already consistent across all years. |
| TSS | `Suspended` | Already consistent. |
| BTEX / volatile organics | `Volatile` | Already consistent. |
| Fecal Coliform | `None` | No fraction applies to a whole-water biological parameter. |

**Why `"Dissolved"` and not `"Filtered, field"` for dissolved metals:**\
Historical data (2000–2013) already uses `"Dissolved"` — no re-upload needed for those years. Using `"Dissolved"` for 2021 and beyond means only the 2021 submission requires correction, and no future split will occur when lab-filtered samples (2023+) are submitted. `"Dissolved"` is the semantically correct WQX fraction value: it identifies *what was measured*, not *how the sample was prepared*.

------------------------------------------------------------------------

## Status by Year

| Years | Dissolved metals fraction in WQP | Action needed |
|----|----|----|
| 2000–2013 | `Dissolved` | ✅ Already correct — no re-upload needed |
| 2021 | `Filtered, field` | 🔧 Corrective CDX re-upload required (see below) |
| 2023+ | *(future submissions)* | ✅ Pipeline will use `Dissolved` |

------------------------------------------------------------------------

## Other Fraction Inconsistencies to Correct

### `Total Recoverable` → `Unfiltered` (historical total metals)

**Affected years:** 2000–2013\
**Affected parameters:** Calcium, Iron, Magnesium\
**Record count:** \~1,709 records

Historical total metals (Ca, Fe, Mg by EPA 200.7) were submitted as `Total Recoverable`. The 2021 pipeline uses `Unfiltered` for the same parameters, and `Unfiltered` will also be correct for 2023+ total metals (200.8 unfiltered). Standardize all years to `Unfiltered`.

### `Total Recoverable` Phosphorus (2013) and `Total Residual` Phosphorus (2010)

**Record counts:** 24 and 1 records respectively\
Correct both to `Total`.

### 2006 Ethylbenzene `Dissolved` fraction

One 2006 Ethylbenzene record uses `Dissolved`. The correct fraction for volatile organics is `Volatile`. Investigate the original 2006 lab report — likely a data entry error.

### 2007 Chromium(VI) `Dissolved` fraction

Ten 2007 Chromium(VI) records use `Dissolved`. Chromium(VI) is a dissolved-phase analyte; `Dissolved` may be intentional here. Verify against the original 2007 lab report before changing. If Chromium(VI) is being reported as its own distinct parameter (separate from total Chromium), `Dissolved` is defensible.

------------------------------------------------------------------------

## Characteristic Name Inconsistencies (Related — Also Requires Correction)

### Retired nitrate/nitrite characteristic name

**In WQP:** `"Inorganic nitrogen (nitrate and nitrite) ***retired***use Nitrate + Nitrite"`\
**Correct current WQX name:** `"Nitrate + Nitrite"` (or `"Total Nitrate/Nitrite-N"` if that is adopted as the project-wide standard — confirm against the 2021 CDX submission)\
**Affected years:** 2007–2008 (2 records)

### Calcium / Iron / Magnesium characteristic name

**Historical WQP values:** `"Calcium"`, `"Iron"`, `"Magnesium"`\
**2021 WQP values:** `"Calcium, Total"`, `"Iron, Total"`, `"Magnesium, Total"`

Decide on a canonical characteristic name for all years and apply it in corrective uploads. Check the current WQX domain list for which form is preferred.

------------------------------------------------------------------------

## CDX Corrective Upload Workflow for 2021 Dissolved Metals

The 2021 dissolved metals records need to be re-uploaded with `ResultSampleFractionText = "Dissolved"` instead of `"Filtered, field"`. This requires a delete-and-resubmit:

1.  **Generate a DELETE file** containing Activity IDs for the 2021 dissolved metals records. Strip the `KENAI_WQX-` org prefix from all Activity IDs (WQX delete files use bare IDs without the org prefix — see `kenai-river-wqx` AGENTS.md session notes from 3/31/2026 for the confirmed working process and CDX UI path).

2.  **Re-render `appendix_a.qmd`** in `kenai-river-wqx` to regenerate `results_activities.csv` with `Dissolved` as the fraction for dissolved metals. The lookup table was updated in April 2026 — the corrected file will be produced automatically on re-render.

3.  **Upload the DELETE file** via CDX, then upload the corrected `results_activities.csv`.

4.  **Verify in WQP** after 24–72 hours that the 2021 records show `Dissolved` for dissolved metals, and confirm in HMW that the full zinc (or other dissolved metal) time series is now visible as a single unified series.

------------------------------------------------------------------------

## What Was Fixed in `kenai-river-wqx` (2021 Pipeline)

-   ✅ `Dissolved` for dissolved metals (200.8) — lookup table updated April 2026 (was `Filtered, field`)\
-   ✅ `Unfiltered` for total metals Ca/Fe/Mg (200.7) — already correct\
-   ✅ `Total` for nutrients — already correct\
-   ✅ `Volatile` for BTEX — already correct\
-   🔧 **Fixed April 2026:** TSS fraction was `NA` due to method code mismatch; explicit `mutate()` added → `Suspended`\
-   🔧 **Fixed April 2026:** FC fraction was `NA` (no lookup table entry); same `mutate()` → `None`

The updated 2021 `results_activities.csv` (from re-render) will include these corrections. A corrective CDX re-upload of 2021 dissolved metals records is needed to apply the `"Filtered, field"` → `"Dissolved"` change in WQP.
