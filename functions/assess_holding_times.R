# assess_holding_times.R
#
# Q17 holding time check. Elapsed time runs from collection to the start of analysis
# (run_date/run_time), the convention used for the limits in sample_holding_times.csv.
# Lab receipt is not part of the calculation; it is only cross-checked, because receipt
# times at the wastewater treatment plants are sometimes hand-written and entered by the
# person operating the template (config.yml).
#
# A missing time of day is handled by bracketing: the earliest and latest possible elapsed
# time are both computed, and the row is Not evaluable only when the answer depends on the
# missing time.
#
# Arguments
#   dat        the dat table (needs dat_row, sample, analyte, lab_name, epa_analysis_id,
#              collect_date, collect_time, run_date, run_time; rec_date and rec_time optional)
#   limits     data frame with result_analytical_method_id and max_holding_time_hours;
#              rows with a blank method id are ignored
#   in_situ    data frame with analytical_method (methods with no holding time: field meters),
#              matched on epa_analysis_id
#
# Returns one row per dat row: hold_status (Pass, Fail, Not evaluable, Not applicable),
# hold_note, elapsed_hours (NA when a time of day is missing), max_hold_hours, receipt_check
# (OK, Out of order, Not recorded, Not applicable). All times are treated as one local clock;
# a daylight saving change between collection and analysis shifts elapsed time by one hour.

assess_holding_times <- function(dat, limits, in_situ) {
  need <- c("dat_row", "sample", "analyte", "lab_name", "epa_analysis_id",
            "collect_date", "collect_time", "run_date", "run_time")
  miss <- setdiff(need, names(dat))
  if (length(miss) > 0) {
    stop("assess_holding_times: dat is missing column(s): ", paste(miss, collapse = ", "),
         call. = FALSE)
  }
  if (!all(c("result_analytical_method_id", "max_holding_time_hours") %in% names(limits))) {
    stop("assess_holding_times: limits needs result_analytical_method_id and ",
         "max_holding_time_hours", call. = FALSE)
  }

  limits <- limits[!is.na(limits$result_analytical_method_id) &
                     trimws(limits$result_analytical_method_id) != "", , drop = FALSE]
  limits$max_holding_time_hours <- as.numeric(limits$max_holding_time_hours)
  if (anyDuplicated(limits$result_analytical_method_id) > 0) {
    stop("assess_holding_times: duplicate method ids in limits", call. = FALSE)
  }

  day_s <- 86400
  secs <- function(d, t) as.numeric(as.Date(d)) * day_s + as.numeric(t)
  # Earliest and latest instant of the day when the time of day is missing
  lo <- function(d, t) ifelse(is.na(t), secs(d, 0), secs(d, t))
  hi <- function(d, t) ifelse(is.na(t), secs(d, day_s - 1), secs(d, t))

  c_lo <- lo(dat$collect_date, dat$collect_time)
  c_hi <- hi(dat$collect_date, dat$collect_time)
  r_lo <- lo(dat$run_date, dat$run_time)
  r_hi <- hi(dat$run_date, dat$run_time)
  min_h <- (r_lo - c_hi) / 3600
  max_h <- (r_hi - c_lo) / 3600
  exact <- !is.na(dat$collect_time) & !is.na(dat$run_time)

  limit_h <- limits$max_holding_time_hours[match(dat$epa_analysis_id,
                                                 limits$result_analytical_method_id)]
  is_in_situ <- dat$epa_analysis_id %in% in_situ$analytical_method

  status <- rep("Not evaluable", nrow(dat))
  note <- rep(NA_character_, nrow(dat))

  no_dates <- is.na(min_h) | is.na(max_h)
  no_limit <- is.na(limit_h) & !is_in_situ
  backwards <- !no_dates & max_h < 0
  fail <- !no_dates & !is.na(limit_h) & min_h > limit_h
  pass <- !no_dates & !is.na(limit_h) & max_h <= limit_h & !backwards

  status[pass] <- "Pass"
  status[fail] <- "Fail"
  status[is_in_situ] <- "Not applicable"
  note[status == "Not evaluable" & no_dates] <- "Collection or analysis date missing"
  note[status == "Not evaluable" & no_limit & !no_dates] <- "No holding time limit for this method"
  note[status == "Not evaluable" & backwards] <- "Analysis precedes collection"
  note[status == "Not evaluable" & is.na(note)] <- "Result depends on a missing time of day"
  note[status == "Fail"] <- sprintf("Hold time exceeded: %.1f h > %g h",
                                    ifelse(exact[status == "Fail"], max_h[status == "Fail"],
                                           min_h[status == "Fail"]),
                                    limit_h[status == "Fail"])
  note[status == "Not applicable"] <- "In-situ measurement"

  # Receipt cross-check: collection <= receipt <= analysis start, when receipt is recorded
  rec_date <- if ("rec_date" %in% names(dat)) dat$rec_date else rep(NA, nrow(dat))
  rec_time <- if ("rec_time" %in% names(dat)) dat$rec_time else rep(NA, nrow(dat))
  rc_lo <- lo(rec_date, rec_time)
  rc_hi <- hi(rec_date, rec_time)
  receipt <- ifelse(is.na(rc_lo), "Not recorded",
                    ifelse(rc_hi < c_lo | rc_lo > r_hi, "Out of order", "OK"))
  receipt[is_in_situ] <- "Not applicable"

  data.frame(
    dat_row = dat$dat_row,
    sample = dat$sample,
    analyte = dat$analyte,
    lab_name = dat$lab_name,
    epa_analysis_id = dat$epa_analysis_id,
    elapsed_hours = ifelse(exact, max_h, NA_real_),
    max_hold_hours = limit_h,
    hold_status = status,
    hold_note = note,
    receipt_check = receipt,
    stringsAsFactors = FALSE
  )
}
