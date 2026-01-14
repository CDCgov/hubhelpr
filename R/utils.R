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


#' Get hub repository name for a given disease.
#'
#' Converts disease identifier to GitHub repository name.
#'
#' @param disease Character. Disease identifier ("covid" or "rsv").
#' @return Character. Repository name.
#' @export
get_hub_repo <- function(disease) {
  checkmate::assert_scalar(disease)
  checkmate::assert_names(disease, subset.of = c("covid", "rsv"))

  dplyr::case_when(
    disease == "covid" ~ "covid19-forecast-hub",
    disease == "rsv" ~ "rsv-forecast-hub"
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

  return(dplyr::case_when(
    disease == "covid" ~ "covid19-forecast-hub",
    disease == "rsv" ~ "rsv-forecast-hub"
  ))
}
