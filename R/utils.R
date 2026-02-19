#' Get NHSN column name for a given disease
#'
#' @param disease Disease name ("covid", "rsv", or "flu")
#' @return Character string with the NHSN column name
#' @export
get_nhsn_col_name <- function(disease) {
  checkmate::assert_choice(disease, choices = c("covid", "rsv", "flu"))

  dplyr::case_when(
    disease == "covid" ~ "totalconfc19newadm",
    disease == "rsv" ~ "totalconfrsvnewadm",
    disease == "flu" ~ "totalconfflunewadm"
  )
}

#' Get NSSP column name for a given disease
#'
#' @param disease Disease name ("covid", "rsv", or "flu")
#' @return Character string with the NSSP column name
#' @export
get_nssp_col_name <- function(disease) {
  checkmate::assert_choice(disease, choices = c("covid", "rsv", "flu"))

  dplyr::case_when(
    disease == "covid" ~ "percent_visits_covid",
    disease == "rsv" ~ "percent_visits_rsv",
    disease == "flu" ~ "percent_visits_flu"
  )
}


#' Get hub display name for a given disease.
#'
#' Converts disease identifier to hub display name format
#' used for identifying hub-baseline and hub-ensemble.
#'
#' @param disease Character. Disease identifier ("covid",
#' "rsv", or "flu").
#' @return Character. Hub name (e.g., "CovidHub", "RSVHub", "FluSight").
#' @export
get_hub_name <- function(disease) {
  checkmate::assert_choice(disease, choices = c("covid", "rsv", "flu"))
  dplyr::case_when(
    disease == "covid" ~ "CovidHub",
    disease == "rsv" ~ "RSVHub",
    disease == "flu" ~ "FluSight"
  )
}


#' Get GitHub repository name for a given disease.
#'
#' Converts disease identifier to corresponding GitHub
#' repository name.
#'
#' @param disease Character. Disease identifier ("covid",
#' "rsv", or "flu").
#' @return Character. GitHub repository name.
#' @export
get_hub_repo_name <- function(disease) {
  checkmate::assert_choice(disease, choices = c("covid", "rsv", "flu"))

  dplyr::case_when(
    disease == "covid" ~ "covid19-forecast-hub",
    disease == "rsv" ~ "rsv-forecast-hub",
    disease == "flu" ~ "FluSight-forecast-hub"
  )
}


#' Get display name for a given disease.
#'
#' Converts disease identifier to human-readable display
#' name.
#'
#' @param disease Character. Disease identifier ("covid",
#' "rsv", or "flu").
#' @return Character. Display name (e.g., "COVID-19", "RSV",
#' "Influenza").
#' @export
get_disease_name <- function(disease) {
  checkmate::assert_choice(disease, choices = c("covid", "rsv", "flu"))

  dplyr::case_when(
    disease == "covid" ~ "COVID-19",
    disease == "rsv" ~ "RSV",
    disease == "flu" ~ "Influenza"
  )
}


#' Round a value to an appropriate place.
#'
#' Rounds values based on magnitude: to nearest 100 for
#' values >= 1000, to nearest 10 for values >= 10,
#' otherwise to nearest integer.
#'
#' @param value Numeric vector. Values to round.
#' @return Numeric vector. Rounded values.
#' @noRd
round_to_place <- function(value) {
  dplyr::case_when(
    value >= 1000 ~ janitor::round_half_up(value, -2),
    value >= 10 ~ janitor::round_half_up(value, -1),
    .default = janitor::round_half_up(value, 0)
  )
}


#' Generate webtext configuration for a target.
#'
#' Returns target-specific text and formatting configuration
#' for webtext generation.
#'
#' @param target Character. Target name (e.g., "wk inc covid hosp").
#' @param disease Character. Disease identifier ("covid" or "rsv").
#' @return List with section_header, target_description, target_short,
#' data_source, value_unit, and format_forecast elements.
#' Returns NULL if target type is not recognized.
#' @noRd
generate_target_webtext_config <- function(target, disease) {
  disease_name <- get_disease_name(disease)

  if (is_hosp_target(target)) {
    config <- list(
      section_header = "Hospital Admissions",
      target_description = glue::glue(
        "new weekly laboratory-confirmed {disease_name} hospital admissions"
      ),
      target_short = glue::glue("{disease_name} hospital admissions"),
      data_source = "NHSN data",
      value_unit = "",
      format_forecast = function(x) round_to_place(x)
    )
  } else if (is_ed_target(target)) {
    config <- list(
      section_header = "ED Visits",
      target_description = glue::glue(
        "the proportion of emergency department visits due to {disease_name}"
      ),
      target_short = glue::glue("{disease_name} ED visit proportions"),
      data_source = "NSSP data",
      value_unit = "%",
      format_forecast = function(x) janitor::signif_half_up(x * 100, 2)
    )
  } else {
    cli::cli_warn("Unknown target type for: {target}, skipping.")
    return(NULL)
  }

  return(config)
}


#' Check if target is a hospital admissions count.
#'
#' Helper function to identify targets that end with "hosp".
#'
#' @param target Character. Target name to check.
#' @return Logical. TRUE if target ends with "hosp", FALSE
#' otherwise.
#' @export
is_hosp_target <- function(target) {
  stringr::str_ends(target, "hosp")
}

#' Check if target is an emergency department visits
#' proportion.
#'
#' Helper function to identify targets that end with
#' "prop ed visits".
#'
#' @param target Character. Target name to check.
#' @return Logical. TRUE if target ends with "prop ed
#' visits", FALSE otherwise.
#' @export
is_ed_target <- function(target) {
  stringr::str_ends(target, "prop ed visits")
}


#' Get unique targets from hub time-series data.
#'
#' Reads the target-data time-series and returns the
#' unique target names present in the data.
#'
#' @param base_hub_path Path to the base hub directory.
#' @return Character vector of unique target names.
#' @export
get_unique_hub_targets <- function(base_hub_path) {
  targets <- hubData::connect_target_timeseries(base_hub_path) |>
    dplyr::distinct(target) |>
    dplyr::collect() |>
    dplyr::pull(target)

  if (length(targets) == 0) {
    cli::cli_abort(
      "No targets found in time-series data."
    )
  }

  return(targets)
}


#' Get human-readable label for a target.
#'
#' Converts a full target name to a human-readable label
#' for use in error messages and assertions.
#'
#' @param target Character. Full target name (e.g.,
#' "wk inc covid hosp", "wk inc rsv prop ed visits").
#' @return Character. Human-readable label.
#' @export
get_target_label <- function(target) {
  dplyr::case_when(
    is_hosp_target(target) ~ "Hospital Admissions",
    is_ed_target(target) ~ "Proportion ED Visits",
    TRUE ~ target
  )
}


#' Get target data type from full target string.
#'
#' Converts full target strings to data type for use in
#' data outputs.
#'
#' @param target Character. Full target name (e.g.,
#' "wk inc covid hosp").
#' @return Character. Target data type ("hosp", "prop_ed",
#' or NA if unrecognized).
#' @export
get_target_data_type <- function(target) {
  dplyr::case_when(
    is_hosp_target(target) ~ "hosp",
    is_ed_target(target) ~ "prop_ed",
    TRUE ~ NA_character_
  )
}
