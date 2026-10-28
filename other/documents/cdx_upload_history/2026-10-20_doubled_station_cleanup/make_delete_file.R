# Builds the DELETE file for the 2021 results stored on the doubled-prefix
# stations (KENAI_WQX-KENAI_WQX-100000xx) on WQP. See README.md in this folder.
# Run from the project root after functions/refresh_wqp_data.R.

library(tidyverse)

out_dir <- "other/output/epa_wqp_uploads/2026-10-20_doubled_station_cleanup"

nr <- read_csv(
  "other/input/WQX_downloads/wqp_live/narrowresult.csv",
  col_types = cols(.default = "c")
)

doubled <- nr |>
  filter(str_starts(MonitoringLocationIdentifier, "KENAI_WQX-KENAI_WQX-"))

# WQX resolves Activity IDs against the logged-in org, so strip the prefix
delete_file <- doubled |>
  distinct(ActivityIdentifier) |>
  mutate(ActivityIdentifier = sub("^KENAI_WQX-", "", ActivityIdentifier))

stopifnot(
  nrow(delete_file) == 830,
  !any(str_starts(delete_file$ActivityIdentifier, "KENAI_WQX-"))
)

# CRLF line endings to match earlier DELETE files that WQX Web accepted
write_csv(
  delete_file,
  file.path(out_dir, "resultphyschem_DELETE_doubled_stations_2021.csv"),
  eol = "\r\n"
)

# Reference list of the doubled stations as shown on WQP
doubled_ids <- doubled |>
  distinct(MonitoringLocationIdentifier) |>
  arrange(MonitoringLocationIdentifier)

write_csv(
  doubled_ids,
  file.path(out_dir, "doubled_stations_to_remove.csv"),
  eol = "\r\n"
)

# Station DELETE file: IDs as WQX stores them (WQP ID minus ONE prefix), e.g.
# "KENAI_WQX-10000002". A bare "10000002" would target an original station.
station_delete <- doubled_ids |>
  transmute(MonitoringLocationIdentifier = sub("^KENAI_WQX-", "", MonitoringLocationIdentifier))

# Original (correct) stations on WQP, and stations in the file to be re-uploaded
original_ids <- read_csv(
  "other/input/WQX_downloads/wqp_live/station.csv",
  col_types = cols(.default = "c")
) |>
  pull(MonitoringLocationIdentifier) |>
  discard(\(x) str_starts(x, "KENAI_WQX-KENAI_WQX-")) |>
  # Convert to the form WQX stores (WQP ID minus one prefix), so both sides
  # of the comparison below are in the same form
  sub(pattern = "^KENAI_WQX-", replacement = "")

reupload_ids <- read_csv(
  "other/output/wqx_formatted/station.csv",
  col_types = cols(.default = "c")
) |>
  pull(MonitoringLocationIdentifier)

stopifnot(
  nrow(station_delete) == 22,
  # Each ID keeps exactly one prefix
  all(str_detect(station_delete$MonitoringLocationIdentifier, "^KENAI_WQX-\\d+$")),
  # None of the IDs may match an original station (compared in WQX-stored form)
  !any(station_delete$MonitoringLocationIdentifier %in% original_ids),
  # The bare originals must not appear in the delete file
  !any(sub("^KENAI_WQX-", "", station_delete$MonitoringLocationIdentifier) %in%
         station_delete$MonitoringLocationIdentifier),
  # Bare IDs to be re-uploaded must not equal anything being deleted
  !any(reupload_ids %in% station_delete$MonitoringLocationIdentifier)
)

write_csv(
  station_delete,
  file.path(out_dir, "monitoringlocation_DELETE_doubled_stations.csv"),
  eol = "\r\n"
)
