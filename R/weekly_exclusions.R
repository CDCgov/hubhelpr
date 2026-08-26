weekly_exclusions_filename <- "exclusions.toml"


#' Path to a hub's weekly location exclusions file.
#'
#' The file lives beside that hub's weekly summaries (
#' each hub has its own file).
#'
#' @param hub_reports_path character, path to the
#' forecast hub reports directory.
#' @param disease character, disease name ("covid" or
#' "rsv").
#'
#' @return The file path, whether or not it exists.
#' @export
weekly_exclusions_path <- function(hub_reports_path, disease) {
  return(fs::path(
    hub_reports_path,
    "weekly-summaries",
    get_hub_repo_name(disease),
    weekly_exclusions_filename
  ))
}


#' Validate one reference date's exclusions.
#'
#' @param exclusions The value parsed from the file for
#' a single date.
#' @param reference_date character, the date it was k
#' eyed under (used for error messages).
#'
#' @return Invisible NULL. Called for its errors.
#' @noRd
assert_valid_weekly_exclusions <- function(exclusions, reference_date) {
  if (is.null(exclusions) || length(exclusions) == 0) {
    return(invisible(NULL))
  }

  if (is.character(exclusions)) {
    assert_valid_location_abbrs(exclusions)
    return(invisible(NULL))
  }

  if (!is.list(exclusions)) {
    cli::cli_abort(
      c(
        "Exclusions for {.val {reference_date}} must be an array of
         abbreviations or a table keyed by target.",
        "x" = "Found {.cls {class(exclusions)}}."
      )
    )
  }

  entry_names <- names(exclusions)
  if (is.null(entry_names) || any(entry_names == "")) {
    cli::cli_abort(
      c(
        "Every key in the exclusions table for {.val {reference_date}}
         must be named.",
        "i" = "Use {.val all} for exclusions that apply to every target."
      )
    )
  }

  purrr::walk(exclusions, \(abbrs) {
    checkmate::assert_character(
      abbrs,
      .var.name = glue::glue("exclusions for {reference_date}")
    )
    assert_valid_location_abbrs(abbrs)
  })

  return(invisible(NULL))
}


#' Read a hub's weekly location exclusions file.
#'
#' The file is TOML keyed by reference date, where each
#' value takes either form the `generate-viz-data`
#' action accepts for `excluded_locations`: an array of
#' abbreviations applying to every target, or a table
#' mapping target names (or `all`) to arrays.
#'
#' ```toml
#' 2025-01-01 = { all = ["VI"], "wk inc covid hosp" = ["GU"] }
#' 2025-02-02 = ["AK", "AR"]
#' ```
#'
#' A date with no entry has no exclusions, so the file
#' lists only the weeks that need one.
#'
#' @param path character, path to the exclusions file. A
#' file that does not exist reads as no exclusions,
#' since a hub that has never needed one has nothing
#' to record.
#'
#' @return Named list of exclusions keyed by reference
#' date, each element in a form accepted by
#' [apply_target_location_exclusions()].
#' @export
read_weekly_exclusions <- function(path) {
  if (!fs::file_exists(path)) {
    return(list())
  }

  parsed <- RcppTOML::parseTOML(path)

  # parseTOML returns a classed list carrying the
  # source path, so strip it to named list
  exclusions <- stats::setNames(
    lapply(seq_along(parsed), \(i) parsed[[i]]),
    names(parsed)
  )

  malformed_dates <- names(exclusions)[
    is.na(lubridate::ymd(names(exclusions), quiet = TRUE))
  ]
  if (length(malformed_dates) > 0) {
    cli::cli_abort(
      c(
        "Every key in {.path {path}} must be a reference date in
         YYYY-MM-DD format.",
        "x" = "Found: {.val {malformed_dates}}."
      )
    )
  }

  purrr::iwalk(exclusions, assert_valid_weekly_exclusions)

  return(exclusions)
}


#' Get a hub's location exclusions for one reference
#' date.
#'
#' @param hub_reports_path character, path to the
#' forecast hub reports directory.
#' @param disease character, disease name ("covid" or
#' "rsv").
#' @param reference_date character or Date, the
#' reference date to look up.
#'
#' @return NULL when the date has no entry, otherwise a
#' character vector or named list of abbreviations,
#' ready to pass as `excluded_locations`.
#' @export
get_weekly_exclusions <- function(
  hub_reports_path,
  disease,
  reference_date
) {
  exclusions <- read_weekly_exclusions(
    weekly_exclusions_path(hub_reports_path, disease)
  )

  entry <- exclusions[[as.character(lubridate::as_date(reference_date))]]

  if (is.null(entry) || length(entry) == 0) {
    return(NULL)
  }

  return(entry)
}


#' Get a hub's location exclusions as JSON for the
#' `generate-viz-data` action.
#'
#' The action takes `excluded_locations` as a JSON
#' string, so this is the bridge from the TOML file
#' to the workflow. TOML is the storage format because
#' it permits comments.
#'
#' @inheritParams get_weekly_exclusions
#'
#' @return A JSON string: `"[]"` when the date has no
#' entry, matching the action's own default.
#' @export
weekly_exclusions_json <- function(
  hub_reports_path,
  disease,
  reference_date
) {
  entry <- get_weekly_exclusions(hub_reports_path, disease, reference_date)

  if (is.null(entry)) {
    return("[]")
  }

  # N.B.: auto_unbox would turn a single abbreviation
  # into a bare string, and the action expects an array
  return(as.character(jsonlite::toJSON(entry, auto_unbox = FALSE)))
}
