#' Normalize excluded locations to a named list.
#'
#' Converts a character vector or named list of excluded
#' locations into a consistent named list format.
#' Validates that all abbreviations are valid US
#' state/territory abbreviations.
#'
#' @param excluded_locations NULL, character vector, or
#' named list of character vectors.
#'
#' @return Named list of character vectors, empty when there is
#' nothing to exclude.
#' @keywords internal
normalize_excluded_locations <- function(excluded_locations) {
  if (is.null(excluded_locations) || length(excluded_locations) == 0) {
    return(list())
  }
  if (is.character(excluded_locations)) {
    assert_valid_location_abbrs(excluded_locations)
    return(list("all" = excluded_locations))
  }
  if (is.list(excluded_locations)) {
    entry_names <- names(excluded_locations)
    if (is.null(entry_names) || any(entry_names == "")) {
      cli::cli_abort(
        c(
          "Every element of {.arg excluded_locations} must be named.",
          "i" = "Use {.val all} for exclusions that apply to every target."
        )
      )
    }
    purrr::walk(excluded_locations, function(x) {
      checkmate::assert_character(
        x,
        .var.name = "excluded_locations list values"
      )
      assert_valid_location_abbrs(x)
    })
    return(excluded_locations)
  }
  cli::cli_abort(
    "{.arg excluded_locations} must be NULL, a character vector, or a named list."
  )
}


#' Assert that location abbreviations are valid.
#'
#' Checks that all provided abbreviations are present
#' in the US location table (from forecasttools).
#' Errors with a message listing any invalid
#' abbreviations.
#'
#' @param abbrs Character vector of abbreviations to
#' validate.
#'
#' @return Invisible NULL. Called for side effects.
#' @noRd
assert_valid_location_abbrs <- function(abbrs) {
  valid_abbrs <- forecasttools::us_location_table$abbr
  invalid <- setdiff(abbrs, valid_abbrs)
  if (length(invalid) > 0) {
    cli::cli_abort(
      "{.arg excluded_locations} contains invalid abbreviation{?s}: {.val {invalid}}."
    )
  }
}


#' Get excluded abbreviations for a specific target.
#'
#' Extracts the abbreviations that should be excluded
#' for a given target from a normalized exclusion list,
#' combining global ("all") exclusions with any
#' target-specific ones.
#'
#' @param exclusions Named list as returned by
#' [normalize_excluded_locations()].
#' @param target Character, the target name.
#'
#' @return Character vector of unique abbreviations to
#' exclude for this target.
#' @noRd
get_target_exclusions <- function(exclusions, target) {
  unique(c(exclusions[["all"]], exclusions[[target]]))
}


#' Build a tibble of excluded location-target pairs
#'
#' @param exclusions Named list as returned by
#' [normalize_excluded_locations()].
#' @param targets Vector of targets for which to build the
#' exclusion tibble.
#' @return [`tibble`][tibble::tibble()] of exclusions that
#' can be anti-joined to data on location and target.
#' @noRd
build_exclusion_df <- function(exclusions, targets) {
  df <- purrr::map(purrr::set_names(targets), \(tgt) {
    get_target_exclusions(exclusions, tgt)
  }) |>
    tibble::enframe(name = "target", value = "location") |>
    tidyr::unnest_longer("location") |>
    dplyr::mutate(
      location = forecasttools::us_location_recode(
        .data$location,
        "abbr",
        "hub"
      )
    )

  return(df)
}

#' Apply target-specific location exclusions to a data
#' frame.
#'
#' Removes rows from a data frame based on
#' target-specific excluded location abbreviations.
#' Supports uniform exclusions (character vector applied
#' to all targets) and target-specific exclusions (named
#' list with target names as keys). Validates target
#' names against hub-supported targets from the hub
#' configuration. Filters on the "target" and "location"
#' columns via anti-join.
#'
#' @param data Data frame with "target" and "location"
#' columns.
#' @param excluded_locations NULL, character vector, or
#' named list of US state/territory abbreviations to
#' exclude. If a character vector, locations are
#' excluded across all targets. If a named list, names
#' should be target names (or "all" for global
#' exclusions) mapping to character vectors of
#' abbreviations.
#' @param base_hub_path Character, path to the forecast
#' hub directory. Used to validate target names against
#' hub-supported targets.
#'
#' @return Data frame with excluded rows removed.
#' @export
apply_target_location_exclusions <- function(
  data,
  excluded_locations,
  base_hub_path
) {
  exclusions <- normalize_excluded_locations(excluded_locations)
  if (length(exclusions) == 0) {
    return(data)
  }

  hub_supported_targets <- get_hub_supported_targets(base_hub_path)
  named_targets <- setdiff(names(exclusions), "all")
  unmatched <- setdiff(named_targets, hub_supported_targets)
  if (length(unmatched) > 0) {
    cli::cli_warn(
      "{.arg excluded_locations} contains target{?s} not in hub config: {.val {unmatched}}."
    )
  }

  exclusion_df <- build_exclusion_df(exclusions, hub_supported_targets)

  return(dplyr::anti_join(
    data,
    exclusion_df,
    by = c("target", "location")
  ))
}


#' Filter data to expected locations only.
#'
#' Only keeps rows where location is in the set of
#' expected US locations minus any excluded locations
#' for that target.
#'
#' @param data Data frame with "target" and "location"
#' columns.
#' @param excluded_locations NULL, character vector, or
#' named list of US state/territory abbreviations to
#' exclude.
#' @param base_hub_path Character, path to the forecast
#' hub directory. Used to determine hub-supported
#' targets.
#' @param expected_locations Character vector of location
#' codes to consider valid. Default:
#' `forecasttools::us_location_table$code`.
#'
#' @return Data frame filtered to expected locations.
#' @noRd
filter_to_expected_locations <- function(
  data,
  excluded_locations,
  base_hub_path,
  expected_locations = forecasttools::us_location_table$code
) {
  normalized <- normalize_excluded_locations(excluded_locations)
  hub_supported_targets <- get_hub_supported_targets(base_hub_path)

  expected_df <- tidyr::crossing(
    target = hub_supported_targets,
    location = expected_locations
  )

  if (length(normalized) > 0) {
    exclusion_df <- build_exclusion_df(normalized, hub_supported_targets)

    expected_df <- dplyr::anti_join(
      expected_df,
      exclusion_df,
      by = c("target", "location")
    )
  }

  return(dplyr::inner_join(
    data,
    expected_df,
    by = c("target", "location")
  ))
}

exclusion_filename <- "report_exclusions.toml"
exclusion_directory <- "config"


#' Path to a hub's weekly location exclusions file.
#'
#' The file lives under `config/`, one per hub.
#'
#' @param hub_reports_path character, path to the
#' forecast hub reports directory.
#' @param disease character, disease name ("covid" or
#' "rsv").
#'
#' @return The file path, whether or not it exists.
#' @noRd
exclusion_file_path <- function(hub_reports_path, disease) {
  return(fs::path(
    hub_reports_path,
    exclusion_directory,
    get_hub_repo_name(disease),
    exclusion_filename
  ))
}


#' Parse a hub's weekly location exclusions file.
#'
#' The file is TOML keyed by reference date, where each value
#' takes either form the `generate-viz-data` action accepts for
#' `excluded_locations`: an array of abbreviations applying to
#' every target, or a table mapping target names (or `all`) to
#' arrays.
#'
#' ```toml
#' 2025-01-01 = { all = ["VI"], "wk inc covid hosp" = ["GU"] }
#' 2025-02-02 = ["AK", "AR"]
#' ```
#'
#' @param path character, path to the exclusions file. A hub with
#' nothing to exclude keeps a file with no entries.
#'
#' @return Named list of exclusions keyed by reference
#' date, each element a named list accepted by
#' [apply_target_location_exclusions()].
#' @export
parse_exclusion_file <- function(path) {
  if (!fs::file_exists(path)) {
    cli::cli_abort(
      c(
        "No exclusions file at {.path {path}}.",
        "i" = "A hub with nothing to exclude still keeps a file with no
               entries, so that a missing file means a broken path rather
               than an empty policy."
      )
    )
  }

  entries <- RcppTOML::parseTOML(path)

  malformed_dates <- names(entries)[
    is.na(lubridate::ymd(names(entries), quiet = TRUE))
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

  return(purrr::imap(entries, \(exclusions, reference_date) {
    rlang::try_fetch(
      normalize_excluded_locations(exclusions),
      error = \(condition) {
        cli::cli_abort(
          "Invalid exclusions for {.val {reference_date}}.",
          parent = condition
        )
      }
    )
  }))
}


#' Get a hub's location exclusions for one reference date.
#'
#' Exclusions are recorded per hub in a TOML file keyed by
#' reference date, where each value takes either form the
#' `generate-viz-data` action accepts for
#' `excluded_locations`: an array of abbreviations applying
#' to every target, or a table mapping target names (or
#' `all`) to arrays.
#'
#' ```toml
#' 2025-01-01 = { all = ["VI"], "wk inc covid hosp" = ["GU"] }
#' 2025-02-02 = ["AK", "AR"]
#' ```
#'
#' A date with no entry has no exclusions, so the file
#' lists only the weeks that need one. The whole file is
#' validated on read, so a typo in a future week's
#' abbreviations surfaces now rather than on the morning
#' that week is generated.
#'
#' @param hub_reports_path character, path to the
#' forecast hub reports directory.
#' @param disease character, disease name ("covid" or
#' "rsv").
#' @param reference_date character or Date, the reference
#' date to look up.
#'
#' @return Named list of abbreviations keyed by target (or
#' by `all`), empty when the date has no entry, ready to
#' pass as `excluded_locations`.
#' @export
get_reference_date_exclusions_list <- function(
  hub_reports_path,
  disease,
  reference_date
) {
  exclusions <- parse_exclusion_file(
    exclusion_file_path(hub_reports_path, disease)
  )

  entry <- exclusions[[as.character(lubridate::as_date(reference_date))]]

  if (is.null(entry)) {
    return(list())
  }

  return(entry)
}


#' Get a hub's location exclusions as JSON for the
#' `generate-viz-data` action.
#'
#' The action takes `excluded_locations` as a JSON string,
#' so this is the bridge from the TOML file to the
#' workflow. TOML is the storage format because it permits
#' comments.
#'
#' @inheritParams get_reference_date_exclusions_list
#'
#' @return A JSON string. An empty entry serializes to
#' `"[]"`, matching the action's own default.
#' @export
get_reference_date_exclusions_json <- function(
  hub_reports_path,
  disease,
  reference_date
) {
  # N.B.: auto_unbox would turn a single abbreviation into a
  # bare string, and the action expects an array
  return(as.character(jsonlite::toJSON(
    get_reference_date_exclusions_list(
      hub_reports_path,
      disease,
      reference_date
    ),
    auto_unbox = FALSE
  )))
}
