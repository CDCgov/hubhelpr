#' Normalize excluded locations to a named list.
#'
#' Converts a character vector or named list of excluded
#' locations into a consistent named list format.
#'
#' @param excluded_locations NULL, character vector, or
#' named list of character vectors.
#'
#' @return Named list of character vectors, or NULL if
#' input is NULL or zero-length.
#' @noRd
normalize_excluded_locations <- function(excluded_locations) {
  if (is.null(excluded_locations) || length(excluded_locations) == 0) {
    return(NULL)
  }
  if (is.character(excluded_locations)) {
    return(list("all" = excluded_locations))
  }
  if (is.list(excluded_locations)) {
    purrr::walk(excluded_locations, function(x) {
      checkmate::assert_character(
        x,
        .var.name = "excluded_locations list values"
      )
    })
    return(excluded_locations)
  }
  cli::cli_abort(
    "{.arg excluded_locations} must be NULL, a character vector, or a named list."
  )
}


#' Build a target-location exclusion data frame.
#'
#' Constructs a tibble of target/location pairs to
#' exclude. Entries keyed by "all" are expanded into
#' one row per supported target. Errors if any named
#' targets in the exclusion list are not in
#' `supported_targets`.
#'
#' @param excluded_locations Named list as returned by
#' `normalize_excluded_locations()`.
#' @param supported_targets Character vector of targets
#' the hub accepts, as returned by
#' `get_hub_supported_targets()`.
#'
#' @return A tibble with columns "target" and "location"
#' (hub codes).
#' @noRd
build_exclusion_df <- function(excluded_locations, supported_targets) {
  named_targets <- setdiff(names(excluded_locations), "all")
  invalid_targets <- setdiff(named_targets, supported_targets)
  if (length(invalid_targets) > 0) {
    cli::cli_abort(
      "{.arg excluded_locations} contains unknown target{?s}: {.val {invalid_targets}}."
    )
  }

  merged <- purrr::map(
    purrr::set_names(supported_targets),
    \(tgt) unique(c(excluded_locations[["all"]], excluded_locations[[tgt]]))
  )

  tibble::enframe(merged, name = "target", value = "location") |>
    tidyr::unnest(cols = "location") |>
    dplyr::mutate(
      location = forecasttools::us_location_recode(
        .data$location,
        "abbr",
        "hub"
      )
    )
}


#' Flatten excluded locations to a character vector.
#'
#' Extracts all unique location abbreviations from an
#' excluded locations specification; some call
#' sites need a flat character vector of abbreviations
#' (e.g., functions operating on single-target data
#' without a target column).
#'
#' @param excluded_locations NULL, character vector, or
#' named list of character vectors.
#'
#' @return Character vector of unique abbreviations, or
#' NULL if input is NULL or zero-length.
#' @noRd
flatten_excluded_locations <- function(excluded_locations) {
  normalized <- normalize_excluded_locations(excluded_locations)
  if (is.null(normalized)) {
    return(NULL)
  }
  unique(unlist(normalized, use.names = FALSE))
}


#' Apply target-specific location exclusions to a data
#' frame.
#'
#' Removes rows from data frame based on target-specific
#' excluded location abbreviations. Supports uniform
#' exclusions (character vector applied to all targets)
#' and target-specific exclusions (named list with target
#' names as keys). Filters on the "target" and
#' "location" columns via anti-join.
#'
#' @param data Data frame with "target" and "location"
#' columns.
#' @param excluded_locations NULL, character vector, or
#' named list of US state/territory abbreviations to
#' exclude. If a character vector, locations are
#' excluded across all targets. If a named list, names
#' should be target names (or "all" for global
#' exclusions) mapping to character vectors of
#' abbreviations. Default: NULL (no exclusions).
#' @param supported_targets Character vector of valid
#' target names, as returned by
#' [get_hub_supported_targets()].
#'
#' @return Data frame with excluded rows removed.
#' @export
apply_target_location_exclusions <- function(
  data,
  excluded_locations,
  supported_targets
) {
  excluded_locations <- normalize_excluded_locations(excluded_locations)
  if (is.null(excluded_locations)) {
    return(data)
  }

  exclusion_df <- build_exclusion_df(excluded_locations, supported_targets)

  dplyr::anti_join(
    data,
    exclusion_df,
    by = c("target", "location")
  )
}
