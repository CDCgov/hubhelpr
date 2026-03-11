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


#' Get GitHub organization owner for a given disease.
#'
#' @param disease Character. Disease identifier ("covid",
#' "rsv", or "flu").
#' @return Character. GitHub organization name.
#' @noRd
get_hub_repo_owner <- function(disease) {
  checkmate::assert_choice(disease, choices = c("covid", "rsv", "flu"))

  dplyr::case_when(
    disease == "covid" ~ "CDCgov",
    disease == "rsv" ~ "CDCgov",
    disease == "flu" ~ "cdcepi"
  )
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



#' Transform a modeling task represented as a nested
#' list to a single data frame.
#'
#' @param task Nested list representing a modeling task,
#' as one entry of the output of
#' [hubUtils::get_round_model_tasks()]. Must have a
#' `target_end_date` specification.
#' @return A [`tibble`][tibble::tibble()] of all
#' potentially valid submittable outputs for the
#' modeling task defined in `task`. Each row of the
#' table represents a single valid forecastable quantity
#' (e.g. "`target` X on `target_end_date` Y in `location`
#' Z"), plus a valid submittable output_type for
#' forecasting that quantity. If multiple `output_type`s
#' are accepted for a given valid forecastable quantity,
#' that quantity will be represented multiple times,
#' with one row for each valid associated `output_type`.
#' @export
flatten_task <- function(task) {
  checkmate::assert_names(
    names(task),
    must.include = c("output_type", "task_ids")
  )
  checkmate::assert_names(
    names(task$task_ids),
    must.include = "target_end_date"
  )
  output_types <- names(task$output_type)

  task_params <- purrr::map(task$task_ids, \(x) c(x$required, x$optional)) |>
    purrr::discard_at(c("horizon", "reference_date"))
  ## discard columns that are redundant with `target_end_date`

  return(do.call(
    tidyr::crossing,
    c(task_params, list(output_type = output_types))
  ))
}


#' Transform a group of modeling tasks represented as a
#' list of nested lists into a single data frame.
#'
#' Calls `flatten_task()` on each entry of the task list.
#'
#' @param task_list List of tasks. Each entry should
#' itself be a nested list that can be passed to
#' `flatten_task()`.
#' @param .deduplicate deduplicate the output if the
#' same flat configuration is found multiple times
#' while flattening the task list? Default `TRUE`.
#'
#' @return A [`tibble`][tibble::tibble()] of all
#' potentially valid submittable outputs for all the
#' modeling tasks defined in `task_list`. Each row of
#' the table represents a single valid forecastable
#' quantity (e.g. "`target` X on `target_end_date` Y in
#' `location` Z"), plus a valid submittable output_type
#' for forecasting that quantity. If multiple
#' `output_type`s are accepted for a given valid
#' forecastable quantity, that quantity will be
#' represented multiple times, with one row for each
#' valid associated `output_type`.
#' @export
flatten_task_list <- function(task_list, .deduplicate = TRUE) {
  flat_tasks <- purrr::map_df(task_list, flatten_task)

  if (.deduplicate) {
    flat_tasks <- dplyr::distinct(flat_tasks)
  }

  return(flat_tasks)
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
  config_tasks <- hubUtils::read_config(base_hub_path, "tasks")
  round_ids <- hubUtils::get_round_ids(config_tasks)

  targets <- purrr::map(round_ids, \(id) {
    hubUtils::get_round_model_tasks(config_tasks, id)
  }) |>
    purrr::map_df(flatten_task_list) |>
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
