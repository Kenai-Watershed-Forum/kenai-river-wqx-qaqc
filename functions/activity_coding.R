# activity_coding.R
#
# Activity ID suffix and Activity Type by sample condition. Used by format_wqx.R.
# Field blanks are real result rows that go to WQX; lab QC never reaches this step.

condition_abbrev <- function(sample_condition) {
  dplyr::case_when(
    sample_condition == "Field Duplicate" ~ "DUP",
    sample_condition == "Field Blank"     ~ "FB",
    sample_condition == "Trip Blank"      ~ "Blank"
  )
}

activity_type <- function(sample_condition, sample_type) {
  dplyr::case_when(
    sample_condition == "Field Duplicate" ~ "Quality Control Field Replicate Msr/Obs",
    sample_condition == "Field Blank"     ~ "Quality Control Sample-Field Blank",
    sample_condition == "Trip Blank"      ~ "Quality Control Sample-Trip Blank",
    sample_type == "TB"                   ~ "Quality Control Sample-Trip Blank",
    TRUE                                  ~ "Field Msr/Obs"
  )
}
