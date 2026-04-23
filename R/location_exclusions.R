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
#' @return Named list of character vectors, or NULL if
#' input is NULL or zero-length.
#' @keywords internal
normalize_excluded_locations <- function(excluded_locations) {
  if (is.null(excluded_locations) || length(excluded_locations) == 0) {
    return(NULL)
  }
  if (is.character(excluded_locations)) {
    assert_valid_location_abbrs(excluded_locations)
    return(list("all" = excluded_locations))
  }
  if (is.list(excluded_locations)) {
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
  if (is.null(exclusions)) {
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

  if (!is.null(normalized)) {
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
