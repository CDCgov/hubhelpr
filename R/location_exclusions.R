#' Normalize excluded locations to a named list.
#'
#' Converts a character vector or named list of excluded
#' locations into a consistent named list format.
#'
#' @param excluded_locations NULL, character vector, or
#' named list of character vectors.
#'
#' @return Named list of character vectors.
#' @noRd
normalize_excluded_locations <- function(excluded_locations) {
  if (is.null(excluded_locations)) {
    return(list())
  }
  if (is.character(excluded_locations)) {
    return(list("all" = excluded_locations))
  }
  if (is.list(excluded_locations)) {
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
#' @param supported_targets character vector of targets
#' the hub accepts, as returned by
#' `get_hub_supported_targets()`.
#'
#' @return A tibble with columns "target" and "location".
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


#' Apply location exclusions to a data frame.
#'
#' Removes rows from a data frame based on excluded
#' location abbreviations. Target-specific
#' exclusions, when `supported_targets` is provided and
#' the data contains a "target" column, are
#' supported; otherwise, exclusions are applied are
#' across all rows.
#'
#' @param data Data frame with a "location" column
#' containing hub-format location codes and, optionally,
#' a "target" column.
#' @param excluded_locations NULL, character vector, or
#' named list of US state/territory abbreviations to
#' exclude. If a character vector, locations are excluded
#' across all rows. If a named list, names should be
#' target names (or "all" for global exclusions) mapping
#' to character vectors of abbreviations. Default: NULL.
#' @param supported_targets Character vector of valid
#' target names. Required for target-specific exclusions
#' when `excluded_locations` is a named list with
#' non-"all" keys. Default: NULL.
#'
#' @return Data frame with excluded rows removed.
#' @export
apply_location_exclusions <- function(
  data,
  excluded_locations,
  supported_targets = NULL
) {
  excluded_locations <- normalize_excluded_locations(excluded_locations)
  if (length(excluded_locations) == 0) {
    return(data)
  }

  if (!is.null(supported_targets) && "target" %in% names(data)) {
    exclusion_df <- build_exclusion_df(
      excluded_locations,
      supported_targets
    )
    data <- dplyr::anti_join(
      data,
      exclusion_df,
      by = c("target", "location")
    )
  } else {
    all_excluded <- unique(unlist(excluded_locations))
    excluded_codes <- forecasttools::us_location_recode(
      all_excluded,
      "abbr",
      "hub"
    )
    data <- dplyr::filter(
      data,
      !(.data$location %in% excluded_codes)
    )
  }

  return(data)
}
