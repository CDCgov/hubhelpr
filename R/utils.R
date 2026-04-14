# named vectors for disease-to-value mappings
nhsn_col_names <- c(
  covid = "totalconfc19newadm",
  rsv = "totalconfrsvnewadm",
  flu = "totalconfflunewadm"
)

nssp_col_names <- c(
  covid = "percent_visits_covid",
  rsv = "percent_visits_rsv",
  flu = "percent_visits_flu"
)

hub_names <- c(
  covid = "CovidHub",
  rsv = "RSVHub",
  flu = "FluSight"
)

hub_repo_names <- c(
  covid = "covid19-forecast-hub",
  rsv = "rsv-forecast-hub",
  flu = "FluSight-forecast-hub"
)

hub_repo_owners <- c(
  covid = "CDCgov",
  rsv = "CDCgov",
  flu = "cdcepi"
)

disease_display_names <- c(
  covid = "COVID-19",
  rsv = "RSV",
  flu = "Influenza"
)


#' Get NHSN column name for a given disease
#'
#' @param disease Disease name ("covid", "rsv", or "flu")
#' @return Character string with the NHSN column name
#' @export
get_nhsn_col_name <- function(disease) {
  rlang::arg_match(disease, names(nhsn_col_names))
  unname(nhsn_col_names[disease])
}

#' Get NSSP column name for a given disease
#'
#' @param disease Disease name ("covid", "rsv", or "flu")
#' @return Character string with the NSSP column name
#' @export
get_nssp_col_name <- function(disease) {
  rlang::arg_match(disease, names(nssp_col_names))
  unname(nssp_col_names[disease])
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
  rlang::arg_match(disease, names(hub_names))
  unname(hub_names[disease])
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
  rlang::arg_match(disease, names(hub_repo_names))
  unname(hub_repo_names[disease])
}


#' Get GitHub organization owner for a given disease.
#'
#' @param disease Character. Disease identifier ("covid",
#' "rsv", or "flu").
#' @return Character. GitHub organization name.
#' @noRd
get_hub_repo_owner <- function(disease) {
  rlang::arg_match(disease, names(hub_repo_owners))
  unname(hub_repo_owners[disease])
}


#' Get GitHub repository URL for a given disease.
#'
#' Constructs full HTTPS GitHub URL for the forecast
#' hub corresponding to the given disease identifier.
#'
#' @param disease Character. Disease identifier ("covid",
#' "rsv", or "flu").
#' @return Character. Full HTTPS GitHub URL.
#' @export
get_hub_repo_url <- function(disease) {
  owner <- get_hub_repo_owner(disease)
  repo_name <- get_hub_repo_name(disease)
  as.character(glue::glue("https://github.com/{owner}/{repo_name}"))
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
  rlang::arg_match(disease, names(disease_display_names))
  unname(disease_display_names[disease])
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


#' Get hub-supported targets from hub configuration.
#'
#' Reads a hub's tasks configuration and extracts all
#' unique target names that the hub accepts.
#'
#' @param base_hub_path Character. Path to the base
#' hub directory containing the hub configuration
#' (tasks.json).
#'
#' @return Character vector of unique supported target
#' names.
#' @export
get_hub_supported_targets <- function(base_hub_path) {
  targets <- get_hub_tasks(base_hub_path) |>
    dplyr::distinct(.data$target) |>
    dplyr::pull()

  if (length(targets) == 0) {
    cli::cli_abort(
      "No targets found in hub configuration (tasks.json)."
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
