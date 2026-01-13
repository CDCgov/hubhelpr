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

  return(dplyr::case_when(
    disease == "covid" ~ "CovidHub",
    disease == "rsv" ~ "RSVHub"
  ))
}

#' Get GitHub repository path for a given disease.
#'
#' Converts disease identifier to corresponding GitHub repository path.
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
