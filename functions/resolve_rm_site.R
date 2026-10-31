# resolve_rm_site.R
#
# Maps lab site labels such as "RM 22", "RM22 DUP", "RM_10_DUP", "RM O" to a
# bare monitoring_location_id by parsing the river-mile number and matching it to
# `river_mile` in reference/baseline_sites.csv (unique across the 22 sites).
#
# Sample condition is read from the label text, never from a list of sites, because
# duplicate, field blank and trip blank locations change every year and season:
#   "DUP"                      -> Field Duplicate
#   "BLANK" with an RM number  -> Field Blank (or Trip Blank when "TRIP" is present)
#   "BLANK" with no RM number  -> lab method blank, lab_qc_type "MB"
#   "POSITIVE"                 -> lab positive control, lab_qc_type "LCS"
#
# Returns a tibble: label, monitoring_location_id (chr, no KENAI_WQX- prefix),
# sample_condition, lab_qc_type. Stops, listing the labels, if a label that is not a
# lab QC row cannot be matched to a site.
#
# Multi-site labels (containing "&", e.g. SGS "RM1.5&6.5 trip blank") are not
# resolved here; they are expanded from config.yml in ingest_sgs().

resolve_rm_site <- function(labels,
                            sites_path = here::here("reference/baseline_sites.csv")) {
  sites <- utils::read.csv(sites_path, check.names = FALSE, stringsAsFactors = FALSE)
  sites <- data.frame(
    monitoring_location_id = sub("^KENAI_WQX-", "", sites$MonitoringLocationIdentifier),
    river_mile             = round(as.numeric(sites$river_mile), 2),
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(sites$river_mile)) {
    stop("river_mile is not unique in ", sites_path, "; resolve_rm_site() needs it to be.",
         call. = FALSE)
  }

  norm <- gsub("_", " ", toupper(trimws(as.character(labels))))
  # Lab typo: capital letter O for zero ("RM O")
  norm <- sub("\\bRM\\s*O\\b", "RM 0", norm)

  multi_site <- grepl("&", norm, fixed = TRUE)
  has_rm     <- grepl("RM\\s*[0-9]", norm) & !multi_site
  rm_num     <- rep(NA_real_, length(norm))
  rm_num[has_rm] <- round(as.numeric(
    sub(".*RM\\s*([0-9]+(?:\\.[0-9]+)?).*", "\\1", norm[has_rm], perl = TRUE)), 2)

  is_trip  <- grepl("TRIP", norm)
  is_blank <- grepl("BLANK", norm)

  lab_qc_type <- dplyr::case_when(
    grepl("POSITIVE", norm)             ~ "LCS",
    is_blank & !has_rm & !is_trip       ~ "MB",
    TRUE                                ~ NA_character_
  )
  sample_condition <- dplyr::case_when(
    !is.na(lab_qc_type)                 ~ NA_character_,
    is_trip & has_rm                    ~ "Trip Blank",
    is_blank & has_rm                   ~ "Field Blank",
    grepl("DUP", norm)                  ~ "Field Duplicate",
    TRUE                                ~ NA_character_
  )

  id <- sites$monitoring_location_id[match(rm_num, sites$river_mile)]
  id[!is.na(lab_qc_type)] <- NA_character_

  unresolved <- is.na(id) & is.na(lab_qc_type)
  if (any(unresolved)) {
    stop("Site labels not matched to a river mile in ", basename(sites_path), ": ",
         paste(unique(labels[unresolved]), collapse = "; "), call. = FALSE)
  }

  tibble::tibble(
    label                  = as.character(labels),
    monitoring_location_id = id,
    sample_condition       = sample_condition,
    lab_qc_type            = lab_qc_type
  )
}
