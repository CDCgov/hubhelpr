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
