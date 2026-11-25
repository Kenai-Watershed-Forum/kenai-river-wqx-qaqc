# WQX domain value lists

Official EPA WQX domain lists, used by `functions/check_wqx_domain.R` to validate values before upload.
Source: `https://cdx.epa.gov/wqx/download/DomainValues/<Name>.CSV`. Download date: see `config.yml` `wqx: domain_download_date`.

To refresh, re-download each file, overwrite it here, and update the date in `config.yml`.

| File | Used by |
|----|----|
| `ActivityMedia.CSV` | Q33 |
| `Characteristic.CSV` | Q33 (only rows with Domain Value Status "Accepted" are valid) |
