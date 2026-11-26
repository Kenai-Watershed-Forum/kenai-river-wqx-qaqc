# WQX domain-list checks. The lists in reference/wqx_domains/ are EPA's own downloads;
# allowed values are never typed by hand in qaqc.qmd.

# Returns the valid names of a domain list (whitespace trimmed). For Characteristic.CSV only
# rows with Domain Value Status "Accepted" count as valid.
read_wqx_domain <- function(domain, dir = here::here("reference/wqx_domains")) {
  d <- utils::read.csv(file.path(dir, paste0(domain, ".CSV")), check.names = FALSE,
                       colClasses = "character", encoding = "UTF-8")
  if ("Domain Value Status" %in% names(d)) d <- d[d[["Domain Value Status"]] == "Accepted", ]
  # ActivityType.CSV labels its name column "Code"; the other lists use "Name"
  unique(trimws(d[[if ("Name" %in% names(d)) "Name" else "Code"]]))
}

# Returns the distinct values (with row counts) that are NA, blank, or not in the domain list.
# Matching is exact and case-sensitive because WQX Web rejects case variants.
check_wqx_domain <- function(values, domain, dir = here::here("reference/wqx_domains")) {
  valid <- read_wqx_domain(domain, dir)
  bad <- is.na(values) | !nzchar(trimws(values)) | !values %in% valid
  tab <- table(ifelse(is.na(values[bad]), "<NA>", values[bad]))
  data.frame(value = names(tab), n_rows = as.integer(tab), stringsAsFactors = FALSE)
}
