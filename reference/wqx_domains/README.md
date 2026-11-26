# WQX domain value lists

Official EPA WQX domain lists, used by `functions/check_wqx_domain.R` to validate values before upload.
Source: `https://cdx.epa.gov/wqx/download/DomainValues/<Name>.CSV`. Download date: see `config.yml` `wqx: domain_download_date`.

To refresh, re-download each file, overwrite it here, and update the date in `config.yml`.

| File | Used by |
|----|----|
| `ActivityMedia.CSV` | Q33 |
| `Characteristic.CSV` | Q33 (only rows with Domain Value Status "Accepted" are valid) |
| `SampleContainerType.CSV`, `SampleContainerColor.CSV` | Q34 |
| `ThermalPreservativeUsed.CSV` | Q34 (domain "Thermal Preservative"; file name differs from the label) |
| `SampleCollectionEquipment.CSV` | Q34 (Equipment ID) |
| `ActivityType.CSV` | Q35 (name column is headed `Code`) |

File names are the domain's internal name, not the label (for example `SampleContainerType`, not `Container Type`). Chemical Preservative has no WQX domain list (free text), so it is not validated here. The older mirror in `C:\_github\kwf\epa_cdx_domains` (January 2024) had identical container, color and thermal lists and 7 fewer equipment names than the online lists.
