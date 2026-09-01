#' Minimum designated submissions required to report an
#' ensemble (this took effect after a particular
#' reference date).
#'
#' The hub reported its ensemble without condition
#' until a minimum number of designated models for
#' reporting was introduced. Regenerating an older
#' report under today's minimum drops ensemble rows
#' that were originally published for the older report;
#' e.g. Missouri loses its proportion of ED visits
#' ensemble on eleven reference dates from
#' `2026-01-17` to `2026-03-28`, because only one
#' designated model submitted that location and target
#' those weeks and today's minimum is two.
#'
#' Each row gives the first reference date the value
#' applied to. A reference date takes the value from
#' the latest row (at or before) it.
#'
#' @format A data frame with two columns:
#' \describe{
#'   \item{reference_date}{Date the minimum took effect}
#'   \item{n_models}{Minimum designated submissions
#'     required, as an integer}
#' }
#' @seealso [n_models_for_ens_reporting_as_of()]
#' @noRd
ensemble_reporting_minimums <- tibble::tibble(
  reference_date = lubridate::as_date(c("2024-11-16", "2026-04-04")),
  n_models = c(0L, 2L)
)


#' Look up the minimum designated submissions required
#' to report an ensemble for a reference date.
#'
#' @param reference_date character or Date, the
#' reference date to look up.
#'
#' @return The minimum, as an integer.
#' @export
n_models_for_ens_reporting_as_of <- function(reference_date) {
  reference_date <- lubridate::as_date(reference_date)

  rule_for_reporting_ensemble_at_the_time <- ensemble_reporting_minimums |>
    dplyr::filter(.data$reference_date <= !!reference_date)

  if (nrow(rule_for_reporting_ensemble_at_the_time) == 0) {
    cli::cli_abort(
      c(
        "No ensemble reporting minimum recorded for
         {.val {as.character(reference_date)}}.",
        "i" = "The record starts at
               {.val {as.character(min(ensemble_reporting_minimums$reference_date))}}."
      )
    )
  }

  return(dplyr::last(rule_for_reporting_ensemble_at_the_time$n_models))
}
