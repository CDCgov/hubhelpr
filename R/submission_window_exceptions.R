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
#' @return The timezone as a string.
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
  timezone <- as.character(timezone)

  if (!timezone %in% OlsonNames()) {
    cli::cli_warn(c(
      "{.val {timezone}} in {.path {admin_path}} is not a known timezone.",
      "i" = "Falling back to UTC. See {.code OlsonNames()} for valid zones."
    ))
    return("UTC")
  }

  return(timezone)
}


#' Parse a hub's submission window exceptions TOML file.
#'
#' This TOML file has a single `dates` array of excepted calendar date
#' exceptions, written as bare TOML dates. Reasons, if wanted, go in comments:
#'
#' ```toml
#' dates = [
#'   2026-01-01, # New Year's Day
#'   2026-07-04, # Independence Day
#'   2026-12-25, # Christmas Day
#' ]
#' ```
#'
#' Every listed date is excepted, so a deadline can be extended across
#' several consecutive days by listing each of them. Permanent changes to the
#' window go in the hub's `submissions_due` configuration instead.
#'
#' @param path character, path to the exceptions file.
#'
#' @return A `Date` vector of excepted dates, sorted and de-duplicated.
#' Length zero when the file does not exist or records no exceptions, since
#' having none is the routine case.
#' @export
parse_submission_window_exceptions_file <- function(path) {
  empty <- lubridate::Date()

  if (!fs::file_exists(path)) {
    return(empty)
  }

  dates <- RcppTOML::parseTOML(path)$dates
  if (is.null(dates) || length(dates) == 0) {
    return(empty)
  }

  return(sort(unique(read_exception_dates(dates, path))))
}


#' Read the `dates` array of an exceptions file as dates.
#'
#' @param dates the `dates` element of a parsed exceptions file.
#' @param path character, path to the file, for error messages.
#'
#' @return A `Date` vector of the same length as `dates`.
#' @noRd
read_exception_dates <- function(dates, path) {
  if (inherits(dates, "Date")) {
    return(dates)
  }

  if (!is.character(dates)) {
    cli::cli_abort(c(
      "{.field dates} in {.path {path}} must be TOML dates.",
      "i" = "Write them bare, e.g. {.code dates = [2026-11-12]}."
    ))
  }

  parsed <- lubridate::ymd(dates, quiet = TRUE)
  if (anyNA(parsed)) {
    cli::cli_abort(c(
      "Every entry of {.field dates} in {.path {path}} must be a calendar
       date in YYYY-MM-DD format.",
      "x" = "Found: {.val {dates[is.na(parsed)]}}."
    ))
  }
  return(parsed)
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

  exceptions <- parse_submission_window_exceptions_file(
    submission_window_exceptions_file_path(base_hub_path)
  )

  if (!date %in% exceptions) {
    return(FALSE)
  }

  cli::cli_inform(c(
    "i" = "Submission window exception for
           {.val {as.character(date)}}."
  ))
  return(TRUE)
}
