#' Get hub name for a given disease.
#'
#' Converts disease identifier to proper hub name format.
#'
#' @param disease Character. Disease identifier ("covid" or "rsv").
#' @return Character. Hub name (e.g., "CovidHub", "RSVHub").
#' @export
get_hub_name <- function(disease) {
  checkmate::assert_scalar(disease)
  checkmate::assert_names(disease, subset.of = c("covid", "rsv"))

  dplyr::case_when(
    disease == "covid" ~ "CovidHub",
    disease == "rsv" ~ "RSVHub"
  )
}


#' Get GitHub repository name for a given disease.
#'
#' Converts disease identifier to corresponding GitHub repository name.
#'
#' @param disease Character. Disease identifier ("covid" or "rsv").
#' @return Character. GitHub repository name.
#' @export
get_hub_repo_name <- function(disease) {
  checkmate::assert_scalar(disease)
  checkmate::assert_names(disease, subset.of = c("covid", "rsv"))

  dplyr::case_when(
    disease == "covid" ~ "covid19-forecast-hub",
    disease == "rsv" ~ "rsv-forecast-hub"
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
    value >= 1000 ~ round(value, -2),
    value >= 10 ~ round(value, -1),
    .default = round(value, 0)
  )
}


#' Check if target is a hospital admissions count.
#'
#' Helper function to identify targets that end with "hosp".
#'
#' @param target Character. Target name to check.
#' @return Logical. TRUE if target ends with "hosp", FALSE otherwise.
#' @export
is_hosp_target <- function(target) {
  stringr::str_ends(target, "hosp")
}

#' Check if target is an emergency department visits proportion.
#'
#' Helper function to identify targets that end with "prop ed visits".
#'
#' @param target Character. Target name to check.
#' @return Logical. TRUE if target ends with "prop ed visits", FALSE otherwise.
#' @export
is_ed_target <- function(target) {
  stringr::str_ends(target, "prop ed visits")
}
