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
