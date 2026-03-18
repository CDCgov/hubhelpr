#' Apply location exclusions to a data frame.
#'
#' Removes rows from a data frame based on excluded
#' location abbreviations. Abbreviations are converted
#' to hub codes internally before filtering.
#'
#' @param data Data frame with a "location" column
#' containing hub-format location codes.
#' @param excluded_locations Character vector of US
#' state/territory abbreviations to exclude, or NULL
#' for no exclusions.
#'
#' @return Data frame with excluded rows removed.
#' @export
apply_location_exclusions <- function(
  data,
  excluded_locations
) {
  if (is.null(excluded_locations) || length(excluded_locations) == 0) {
    return(data)
  }
  checkmate::assert_character(excluded_locations)

  excluded_codes <- forecasttools::us_location_recode(
    excluded_locations,
    "abbr",
    "hub"
  )

  dplyr::filter(
    data,
    !(.data$location %in% excluded_codes)
  )
}
