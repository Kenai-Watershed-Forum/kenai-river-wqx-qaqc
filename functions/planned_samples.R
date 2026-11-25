# planned_samples.R
#
# Reads the year's planned-samples workbook (year_files/planned_samples.xlsx, built by
# build_planned_samples_template.R), checks it, expands it into one row per planned
# result, and compares that plan with `dat`.
#
#   read_planned_samples(path, groups_path)     -> list(rules, qc, sites, groups), all text
#   check_planned_samples(plan)                 -> stops listing every unanswered or invalid cell
#   expand_planned_samples(plan)                -> tibble, one row per planned result
#   compare_planned_to_dat(planned, dat, spring_date, summer_date)
#                                               -> tibble, one row per season/site/analyte/sample type
#
# A result is one analyte (and fraction, for metals) at one site, season and sample type
# (Primary, Field Duplicate, Field Blank, Trip Blank). Planned field QC samples count toward
# expected results; laboratory QC does not (it is not in `dat`). Results collected but not
# planned are reported as `unplanned` and are not part of the expected count.
#
# Nothing here is year-specific: sampling dates come from config.yml via the caller.

planned_seasons <- c("spring", "summer")
planned_qc_types <- c("Field Duplicate", "Field Blank", "Trip Blank")

# Splits "1.5;6.5" into c(1.5, 6.5); NA or blank gives numeric(0). Unparseable parts give NA.
parse_rm <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(numeric(0))
  round(suppressWarnings(as.numeric(trimws(strsplit(x, ";", fixed = TRUE)[[1]]))), 2)
}

read_planned_samples <- function(path = here::here("year_files/planned_samples.xlsx"),
                                 groups_path = here::here("reference/analyte_groups.csv")) {
  rd <- function(sheet) {
    readxl::read_excel(path, sheet = sheet, col_types = "text") |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(x) dplyr::na_if(trimws(x), "")))
  }
  list(
    rules  = rd("analyte_rules"),
    qc     = rd("qc_samples"),
    sites  = rd("site_list") |> dplyr::mutate(river_mile = round(as.numeric(river_mile), 2)),
    groups = utils::read.csv(groups_path, stringsAsFactors = FALSE, na.strings = "") |>
      tibble::as_tibble()
  )
}

check_planned_samples <- function(plan) {
  problems <- character()
  bad <- function(...) problems <<- c(problems, paste0(...))
  rules <- plan$rules; qc <- plan$qc; sites <- plan$sites; groups <- plan$groups

  need <- c("analyte_group", "analyte", "sample_fraction", "spring", "summer", "site_rule",
            "rm_min", "rm_max", "add_rm", "exclude_rm")
  miss <- setdiff(need, names(rules))
  if (length(miss)) stop("planned_samples: analyte_rules is missing column(s): ",
                         paste(miss, collapse = ", "), call. = FALSE)
  need_qc <- c("season", "sample_type", "river_mile", "analyte_group", "confirm")
  miss <- setdiff(need_qc, names(qc))
  if (length(miss)) stop("planned_samples: qc_samples is missing column(s): ",
                         paste(miss, collapse = ", "), call. = FALSE)

  for (i in seq_len(nrow(rules))) {
    lab <- paste0("analyte_rules row ", i + 1, " (", rules$analyte[i], ", ",
                  rules$analyte_group[i], ")")
    for (s in planned_seasons) {
      if (!rules[[s]][i] %in% c("Y", "N")) bad(lab, ": ", s, " must be Y or N")
    }
    if (!rules$site_rule[i] %in% c("all", "range", "list")) {
      bad(lab, ": site_rule must be all, range or list")
    }
    rmin <- rules$rm_min[i]; rmax <- rules$rm_max[i]
    if (rules$site_rule[i] %in% "range") {
      if (is.na(rmin) && is.na(rmax)) bad(lab, ": a range rule needs rm_min or rm_max")
      for (v in c(rmin, rmax)) if (!is.na(v) && is.na(suppressWarnings(as.numeric(v)))) {
        bad(lab, ": rm_min and rm_max must be numbers")
      }
    }
    if (rules$site_rule[i] %in% "list" && !length(parse_rm(rules$add_rm[i]))) {
      bad(lab, ": a list rule needs river miles in add_rm")
    }
    for (col in c("add_rm", "exclude_rm")) {
      v <- parse_rm(rules[[col]][i])
      if (anyNA(v) || any(!v %in% sites$river_mile)) {
        bad(lab, ": ", col, " has a river mile that is not in site_list")
      }
    }
    key <- paste(rules$analyte[i], rules$analyte_group[i], rules$sample_fraction[i])
    if (!key %in% paste(groups$analyte, groups$analyte_group, groups$sample_fraction)) {
      bad(lab, ": not found in reference/analyte_groups.csv")
    }
  }

  for (i in seq_len(nrow(qc))) {
    lab <- paste0("qc_samples row ", i + 1, " (", qc$season[i], " ", qc$sample_type[i], ")")
    if (!qc$season[i] %in% planned_seasons) bad(lab, ": season must be spring or summer")
    if (!qc$sample_type[i] %in% planned_qc_types) {
      bad(lab, ": sample_type must be one of ", paste(planned_qc_types, collapse = ", "))
    }
    rm <- parse_rm(qc$river_mile[i])
    if (!length(rm)) {
      bad(lab, ": river_mile is blank")
    } else if (anyNA(rm) || any(!rm %in% sites$river_mile)) {
      bad(lab, ": river_mile has a value that is not in site_list")
    }
    if (!qc$confirm[i] %in% "Y") bad(lab, ": confirm must be Y")
    g <- if (is.na(qc$analyte_group[i])) character() else
      trimws(strsplit(qc$analyte_group[i], ";", fixed = TRUE)[[1]])
    if (!length(g)) {
      bad(lab, ": analyte_group is blank")
    } else if (any(g == "All analytes") && !qc$sample_type[i] %in% "Field Duplicate") {
      bad(lab, ": All analytes applies only to Field Duplicate")
    } else if (any(!g %in% c("All analytes", groups$analyte_group))) {
      bad(lab, ": analyte_group not found in reference/analyte_groups.csv")
    }
  }

  if (length(problems)) {
    stop("planned_samples.xlsx needs attention before the QA/QC run:\n  ",
         paste(problems, collapse = "\n  "), call. = FALSE)
  }
  invisible(plan)
}

expand_planned_samples <- function(plan) {
  rules <- plan$rules; qc <- plan$qc; sites <- plan$sites
  id_of <- stats::setNames(sites$monitoring_location_id, sites$river_mile)

  primary <- list()
  for (i in seq_len(nrow(rules))) {
    for (s in planned_seasons) {
      if (!rules[[s]][i] %in% "Y") next
      rm <- switch(rules$site_rule[i],
        all   = sites$river_mile,
        range = {
          lo <- suppressWarnings(as.numeric(rules$rm_min[i]))
          hi <- suppressWarnings(as.numeric(rules$rm_max[i]))
          sites$river_mile[(is.na(lo) | sites$river_mile >= lo) & (is.na(hi) | sites$river_mile <= hi)]
        },
        list  = numeric(0))
      rm <- setdiff(union(rm, parse_rm(rules$add_rm[i])), parse_rm(rules$exclude_rm[i]))
      if (!length(rm)) next
      primary[[length(primary) + 1]] <- tibble::tibble(
        season = s, river_mile = rm, analyte = rules$analyte[i],
        sample_fraction = rules$sample_fraction[i], analyte_group = rules$analyte_group[i],
        sample_type = "Primary", source = "analyte_rules")
    }
  }
  primary <- dplyr::bind_rows(primary)

  qc_rows <- list()
  for (i in seq_len(nrow(qc))) {
    groups_i <- trimws(strsplit(qc$analyte_group[i], ";", fixed = TRUE)[[1]])
    for (rm in parse_rm(qc$river_mile[i])) {
      base <- if ("All analytes" %in% groups_i) {
        primary[primary$season == qc$season[i] & primary$river_mile == rm, ]
      } else {
        # Blanks are analyzed for the group's analytes planned that season, at any site
        dplyr::distinct(primary[primary$season == qc$season[i] &
                                  primary$analyte_group %in% groups_i, ],
                        analyte, sample_fraction, analyte_group)
      }
      if (!nrow(base)) next
      qc_rows[[length(qc_rows) + 1]] <- tibble::tibble(
        season = qc$season[i], river_mile = rm, analyte = base$analyte,
        sample_fraction = base$sample_fraction, analyte_group = base$analyte_group,
        sample_type = qc$sample_type[i], source = paste0("qc_samples row ", i + 1))
    }
  }

  out <- dplyr::bind_rows(primary, dplyr::bind_rows(qc_rows))
  out$monitoring_location_id <- unname(id_of[as.character(out$river_mile)])
  out |>
    dplyr::select(season, sample_type, river_mile, monitoring_location_id, analyte,
                  sample_fraction, analyte_group, source)
}

compare_planned_to_dat <- function(planned, dat, spring_date, summer_date) {
  spring_date <- as.Date(spring_date); summer_date <- as.Date(summer_date)
  # Fraction only distinguishes results for analytes the plan splits by fraction
  frac_analytes <- unique(planned$analyte[!is.na(planned$sample_fraction)])
  nearest <- function(d) ifelse(abs(as.numeric(d - spring_date)) <= abs(as.numeric(d - summer_date)),
                                "spring", "summer")

  p <- planned |>
    dplyr::mutate(frac_key = dplyr::coalesce(sample_fraction, "")) |>
    dplyr::summarise(planned = dplyr::n(), analyte_group = dplyr::first(analyte_group),
                     .by = c(season, sample_type, monitoring_location_id, analyte, frac_key))

  has_status <- "qc_status" %in% names(dat)
  r <- dat |>
    dplyr::mutate(
      season = nearest(collect_date),
      sample_type = dplyr::coalesce(sample_condition, "Primary"),
      frac_key = dplyr::if_else(analyte %in% frac_analytes,
                                dplyr::coalesce(result_sample_fraction, ""), ""),
      is_accepted = if (has_status) qc_status %in% "Accepted" else TRUE
    ) |>
    dplyr::summarise(received = dplyr::n(), accepted = sum(is_accepted),
                     .by = c(season, sample_type, monitoring_location_id, analyte, frac_key))

  dplyr::full_join(p, r, by = c("season", "sample_type", "monitoring_location_id",
                                "analyte", "frac_key")) |>
    dplyr::mutate(
      dplyr::across(c(planned, received, accepted), \(x) dplyr::coalesce(x, 0L)),
      missing   = pmax(planned - received, 0L),
      unplanned = pmax(received - planned, 0L),
      # Accepted results against the plan only; extras never raise the numerator
      accepted_planned = pmin(accepted, planned),
      sample_fraction = dplyr::na_if(frac_key, "")
    ) |>
    dplyr::select(season, sample_type, monitoring_location_id, analyte, sample_fraction,
                  analyte_group, planned, received, accepted, accepted_planned, missing,
                  unplanned)
}
