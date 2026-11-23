#' Path to a hub's record of submission window exceptions (default: in
#' `hub-config`).
#'
#' @param base_hub_path character, path to the hub directory.
#' @param directory character, directory under `base_hub_path` holding hub
#' configuration. Default: "hub-config".
#' @param filename character, name of the record within the hub's
#' configuration directory. Default: "submission_window_exceptions.toml".
#'
#' @return The file path, whether or not it exists.
#' @noRd
submission_window_exceptions_file_path <- function(
  base_hub_path,
  directory = "hub-config",
  filename = "submission_window_exceptions.toml"
) {
  return(fs::path(base_hub_path, directory, filename))
}


#' A hub's configured timezone (from `hub-config/admin.json`).
#'
#' @param base_hub_path character, path to the hub directory.
#'
#' @return The timezone as a string. "UTC" when `admin.json` is missing or
#' declares no timezone.
#' @noRd
hub_timezone <- function(base_hub_path) {
  admin_path <- fs::path(base_hub_path, "hub-config", "admin.json")
  if (!fs::file_exists(admin_path)) {
    return("UTC")
  }
  timezone <- jsonlite::read_json(admin_path)$timezone
  if (is.null(timezone)) {
    return("UTC")
  }
  return(as.character(timezone))
}


#' Parse a hub's submission window exceptions TOML file.
#'
#' This TOML file is keyed by the  calendar date where submissions dates are
#' exceptions. Values here are reasons the exception was granted, e.g.:
#'
#' ```toml
#' 2026-11-12 = "NHSN publication delayed past the Wednesday deadline"
#' ```
#'
#' Exceptions are one-off. Standing changes to the window instead. go in the
#' hub's `submissions_due` configuration. Only the most recent excepted date
#' is returned.
#'
#' @param path character, path to the exceptions file.
#'
#' @return The latest excepted date, as a `Date` of length one. `NA` of class
#' `Date` when the file does not exist or records no exceptions, since
#' having none is the routine case.
#' @export
parse_submission_window_exceptions_file <- function(path) {
  empty <- lubridate::NA_Date_

  if (!fs::file_exists(path)) {
    return(empty)
  }

  contents <- RcppTOML::parseTOML(path)
  if (length(contents) == 0) {
    return(empty)
  }

  exceptions <- contents |>
    tibble::enframe(name = "date_raw", value = "reason") |>
    tidyr::unnest("reason") |>
    dplyr::mutate(date = lubridate::ymd(.data$date_raw, quiet = TRUE))

  malformed_dates <- exceptions |>
    dplyr::filter(is.na(.data$date)) |>
    dplyr::pull("date_raw")

  if (length(malformed_dates) > 0) {
    cli::cli_abort(
      c(
        "Every key in {.path {path}} must be a calendar date in
         YYYY-MM-DD format.",
        "x" = "Found: {.val {malformed_dates}}."
      )
    )
  }

  checkmate::assert_character(
    exceptions$reason,
    min.chars = 1,
    any.missing = FALSE,
    .var.name = glue::glue("reasons in {path}")
  )

  return(max(exceptions$date))
}


#' Check if a date is an excepted submission date for
#' a hub.
#'
#' Factors into the `skip_submit_window_check` input of the hubverse
#' `validate-submission` action.
#'
#' Absence of an exceptions file means no exceptions, so proceed routinely.
#'
#' @param base_hub_path character, path to the hub directory.
#' @param date character or Date, the date to look up. Default is today in the
#' hub's configured timezone.
#'
#' @return `TRUE` when `date` is recorded as an exception, otherwise `FALSE`.
#' @export
is_submission_window_exception <- function(
  base_hub_path,
  date = NULL
) {
  checkmate::assert_string(base_hub_path)

  if (is.null(date)) {
    date <- lubridate::as_date(
      lubridate::now(tzone = hub_timezone(base_hub_path))
    )
  }
  date <- lubridate::as_date(date)

  latest_exception <- parse_submission_window_exceptions_file(
    submission_window_exceptions_file_path(base_hub_path)
  )

  if (is.na(latest_exception) || latest_exception != date) {
    return(FALSE)
  }

  cli::cli_inform(c(
    "i" = "Submission window exception for
           {.val {as.character(date)}}."
  ))
  return(TRUE)
}
