# check_cdx_ids.R
#
# Final ID guard for the CDX upload files. Stops with a message listing every
# problem, so nothing is written when an ID would be rejected or misfiled by WQX.
#
# WQX Web prepends the organization prefix (KENAI_WQX-) to IDs on import, so upload
# files must hold bare IDs. A prefixed Monitoring Location ID once created 22 doubled
# stations (KENAI_WQX-KENAI_WQX-100000xx).
#
# Arguments
#   results      data frame with columns `Monitoring Location ID` and `Activity ID`
#   station      data frame with column `MonitoringLocationIdentifier`
#   known_sites  data frame with columns `monitoring_location_id` and `site_code`
#                (the `adec_site_names` sheet); the only source of valid IDs
#
# Checks
#   1. no ID column value starts with KENAI_WQX
#   2. every results Location ID is in known_sites
#   3. the station file holds exactly the known_sites IDs
#   4. every Activity ID starts with the site code of its own Location ID
#   5. no Activity ID has an NA component (e.g. an unmatched analyte abbreviation)
#
# Returns TRUE invisibly when all checks pass.

check_cdx_ids <- function(results, station, known_sites) {
  need <- function(df, cols, what) {
    miss <- setdiff(cols, names(df))
    if (length(miss)) stop(what, " is missing column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  }
  need(results, c("Monitoring Location ID", "Activity ID"), "results")
  need(station, "MonitoringLocationIdentifier", "station")
  need(known_sites, c("monitoring_location_id", "site_code"), "known_sites")

  loc <- as.character(results[["Monitoring Location ID"]])
  act <- as.character(results[["Activity ID"]])
  stn <- as.character(station[["MonitoringLocationIdentifier"]])
  known_ids <- as.character(known_sites$monitoring_location_id)

  problems <- character()
  report <- function(msg, x) {
    x <- unique(x)
    more <- if (length(x) > 5) paste0(" (and ", length(x) - 5, " more)") else ""
    problems <<- c(problems, paste0(msg, ": ", paste(utils::head(x, 5), collapse = ", "), more))
  }

  prefixed <- function(x) !is.na(x) & grepl("^KENAI_WQX", x)
  if (any(prefixed(loc))) report("results `Monitoring Location ID` starts with KENAI_WQX", loc[prefixed(loc)])
  if (any(prefixed(act))) report("results `Activity ID` starts with KENAI_WQX", act[prefixed(act)])
  if (any(prefixed(stn))) report("station `MonitoringLocationIdentifier` starts with KENAI_WQX", stn[prefixed(stn)])

  unknown <- is.na(loc) | !loc %in% known_ids
  if (any(unknown)) report("results Location ID not in the known site list", loc[unknown])

  if (length(setdiff(known_ids, stn))) report("station file is missing sites", setdiff(known_ids, stn))
  if (length(setdiff(stn, known_ids))) report("station file has sites not in the known list", setdiff(stn, known_ids))

  expected <- known_sites$site_code[match(loc, known_ids)]
  bad_site <- is.na(act) | is.na(expected) | !startsWith(act, paste0(expected, "-"))
  # Rows with an unknown Location ID are already reported above
  bad_site <- bad_site & !unknown
  if (any(bad_site)) report("Activity ID does not start with its site code", act[bad_site])

  has_na <- !is.na(act) & grepl("(^|-)NA(-|$)", act)
  if (any(has_na)) report("Activity ID contains an NA component", act[has_na])

  if (length(problems)) {
    stop("CDX ID check failed:\n- ", paste(problems, collapse = "\n- "), call. = FALSE)
  }
  invisible(TRUE)
}
