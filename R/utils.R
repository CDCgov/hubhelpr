#' Get hub display name for a given disease.
#'
#' Converts disease identifier to hub display name format used for
#' identifying hub-baseline and hub-ensemble.
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
