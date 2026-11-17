# assign_qc_status.R
#
# Assigns Accepted/Rejected to each sample result in `dat` from the assessed lab QC table
# (assess_lab_qc() output) and the batch table from combine_dat(). Rejected means the
# project team's own designation: the result still goes to WQX, clearly marked.
#
# A failing lab QC row (qc_result == "Fail") rejects sample results by scope:
#   batch    LCS, LCSD, CCV, ICV, CVC, QCS, surrogates on control samples, LLQC, and any other
#            row that is not a blank or sample-specific: every result in the same batch with
#            the same lab, method, analyte and fraction. Surrogate analytes match on method only.
#   blank    detected sample results in the same batch, lab, method, analyte and fraction that
#            are below `blank_multiple` x the blank result. The QAPP does not set a multiple;
#            it is a project convention (config.yml qc$blank_multiple).
#   parent   MS/MSD: the parent sample only (lab sample ID parsed from the leading digits of
#            the MS sample_id), same analyte, method and fraction. A matrix spike failure with
#            passing batch controls is matrix-specific. Parents that are another client's
#            sample match nothing and are listed in lab_qc_actions with n_matched 0.
#   sample   surrogates on a field sample: all results of that lab sample with the same method.
# Not evaluable rows never reject. Rows with no batch ID (for example YSI) are never matched
# by the batch and blank scopes.
#
# Batch linking: a QC row matches a sample result when the analytical batch IDs are equal
# or the prep batch IDs are equal (non-missing IDs only).
#
# Arguments
#   dat             contract table (reference/dat_contract.md)
#   sample_batches  lab_name, lab_sample, collect_date, analyte, analytical_method, dissolved,
#                   anl_batch, prp_batch; one row per key
#   lab_qc          assess_lab_qc() output
#   blank_multiple  positive number (config$qc$blank_multiple)
# Returns list(dat, lab_qc_actions)
#   dat              input plus qc_status ("Accepted"/"Rejected") and qc_fail_reason
#   lab_qc_actions   one row per failing lab QC row: scope, n_matched, n_rejected

assign_qc_status <- function(dat, sample_batches, lab_qc, blank_multiple) {
  suppressPackageStartupMessages({ library(dplyr); library(stringr) })
  stopifnot(is.numeric(blank_multiple), length(blank_multiple) == 1, blank_multiple > 0)

  need <- function(x, cols, nm) {
    miss <- setdiff(cols, names(x))
    if (length(miss) > 0) stop("assign_qc_status: ", nm, " is missing column(s): ",
                               paste(miss, collapse = ", "), call. = FALSE)
  }
  sb_key <- c("lab_name", "lab_sample", "collect_date", "analyte", "analytical_method", "dissolved")
  need(dat, c(sb_key, "result", "resultflag", "units"), "dat")
  need(sample_batches, c(sb_key, "anl_batch", "prp_batch"), "sample_batches")
  need(lab_qc, c("sample_type", "qc_group", "qc_check", "qc_result", "analyte", "analytical_method",
                 "result", "units", "anl_batch", "prp_batch", "lab_name", "lab_sample_id",
                 "sample_id", "dissolved"), "lab_qc")

  sb <- sample_batches |> mutate(lab_sample = as.character(lab_sample))
  if (anyDuplicated(sb[sb_key]) > 0) {
    stop("assign_qc_status: sample_batches has more than one row for the same sample key",
         call. = FALSE)
  }

  d <- dat |>
    mutate(.row = row_number(), lab_sample = as.character(lab_sample)) |>
    left_join(sb, by = sb_key) |>
    select(.row, lab_name, lab_sample, analyte, analytical_method, dissolved, result, resultflag,
           units, s_anl = anl_batch, s_prp = prp_batch)

  surr_pat <- "\\(surr\\)"
  batch_control_types <- c("MB", "LCS", "LCSD", "CCV", "ICV", "CVC", "QCS", "ICB", "CB", "IB",
                           "LLQC", "MS", "MSD", "OS")

  f <- lab_qc |>
    mutate(.qc_id = row_number(), lab_sample_id = as.character(lab_sample_id)) |>
    filter(qc_result == "Fail") |>
    mutate(
      scope = case_when(
        qc_group == "blank"                                           ~ "blank",
        sample_type %in% c("MS", "MSD")                               ~ "parent",
        qc_group == "surrogate" & !sample_type %in% batch_control_types ~ "sample",
        TRUE                                                          ~ "batch"
      ),
      parent = str_extract(sample_id, "^[0-9]+"),
      batch_label = coalesce(anl_batch, prp_batch, "none")
    )

  batch_ok <- function(q_anl, s_anl, q_prp, s_prp) {
    (!is.na(q_anl) & !is.na(s_anl) & q_anl == s_anl) |
      (!is.na(q_prp) & !is.na(s_prp) & q_prp == s_prp)
  }
  empty <- tibble(.qc_id = integer(), .row = integer(), reason = character(), rejected = logical())

  # Batch and blank scopes
  qb <- f |> filter(scope %in% c("batch", "blank")) |>
    select(.qc_id, scope, lab_name, analytical_method, dissolved, q_analyte = analyte,
           q_anl = anl_batch, q_prp = prp_batch, q_result = result, q_units = units,
           sample_type, qc_check, batch_label)
  m_batch <- if (nrow(qb) == 0) empty else {
    m <- qb |>
      inner_join(d, by = c("lab_name", "analytical_method", "dissolved"),
                 relationship = "many-to-many") |>
      filter(batch_ok(q_anl, s_anl, q_prp, s_prp),
             grepl(surr_pat, q_analyte, ignore.case = TRUE) | q_analyte == analyte)
    is_blank <- m$scope == "blank"
    detected <- !is.na(m$result) & !m$resultflag %in% c("U", "ND")
    if (any(is_blank & detected & !is.na(m$q_units) & !is.na(m$units) & m$q_units != m$units)) {
      stop("assign_qc_status: a failing blank and the sample results it covers have different units",
           call. = FALSE)
    }
    m |> mutate(
      rejected = !is_blank | (detected & !is.na(q_result) & result < blank_multiple * q_result),
      reason = if_else(
        is_blank,
        sprintf("Blank fail (%s %s %s, batch %s): result below %gx blank",
                sample_type, format(q_result), q_units, batch_label, blank_multiple),
        sprintf("Lab QC fail (%s %s, batch %s)", sample_type, qc_check, batch_label))
    ) |> select(.qc_id, .row, reason, rejected)
  }

  # Parent scope (MS/MSD)
  qp <- f |> filter(scope == "parent") |>
    select(.qc_id, lab_name, analytical_method, dissolved, q_analyte = analyte, parent,
           sample_type, qc_check)
  m_parent <- if (nrow(qp) == 0) empty else {
    qp |>
      inner_join(d, by = c("lab_name", "analytical_method", "dissolved"),
                 relationship = "many-to-many") |>
      filter(!is.na(parent), lab_sample == parent, analyte == q_analyte) |>
      mutate(rejected = TRUE,
             reason = sprintf("Lab QC fail (%s %s on this sample)", sample_type, qc_check)) |>
      select(.qc_id, .row, reason, rejected)
  }

  # Sample scope (surrogates on field samples)
  qs <- f |> filter(scope == "sample") |>
    select(.qc_id, lab_name, analytical_method, lab_sample_id, sample_type, qc_check)
  m_sample <- if (nrow(qs) == 0) empty else {
    qs |>
      inner_join(d, by = c("lab_name", "analytical_method"), relationship = "many-to-many") |>
      filter(!is.na(lab_sample_id), lab_sample == lab_sample_id) |>
      mutate(rejected = TRUE,
             reason = sprintf("Lab QC fail (surrogate %s on this sample)", qc_check)) |>
      select(.qc_id, .row, reason, rejected)
  }

  m_all <- bind_rows(m_batch, m_parent, m_sample)

  by_row <- m_all |>
    filter(rejected) |>
    group_by(.row) |>
    summarise(qc_fail_reason = paste(unique(reason), collapse = "; "), .groups = "drop")

  out <- dat |>
    mutate(.row = row_number()) |>
    left_join(by_row, by = ".row") |>
    mutate(qc_status = if_else(is.na(qc_fail_reason), "Accepted", "Rejected")) |>
    select(-.row)

  counts <- m_all |>
    group_by(.qc_id) |>
    summarise(n_matched = n(), n_rejected = sum(rejected), .groups = "drop")
  actions <- f |>
    left_join(counts, by = ".qc_id") |>
    mutate(n_matched = coalesce(n_matched, 0L), n_rejected = coalesce(n_rejected, 0L)) |>
    select(sample_type, qc_group, qc_check, analyte, analytical_method, sample_id, anl_batch,
           prp_batch, scope, n_matched, n_rejected)

  list(dat = out, lab_qc_actions = actions)
}
