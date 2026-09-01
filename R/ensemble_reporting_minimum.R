#' Minimum designated submissions required to report an
#' ensemble, by the reference date the rule took effect.
#'
#' The hub reported its ensemble unconditionally until a
#' minimum was introduced, so the rule in force depends on
#' the week. Regenerating an older report under today's
#' minimum drops ensemble rows that were legitimately
#' published: Missouri loses its proportion of ED visits
#' ensemble on eleven reference dates from `2026-01-17` to
#' `2026-03-28`, because only one designated model
#' submitted that location and target those weeks and
#' today's minimum is two.
#'
#' Each row gives the first reference date the value
#' applied to. A reference date takes the value from the
#' latest row at or before it.
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


#' Look up the minimum designated submissions required to
#' report an ensemble for a reference date.
#'
#' Returns the rule in force that week, so that
#' regenerating an old report applies the minimum it was
#' published under rather than today's.
#'
#' The boundary is observable in the published reports.
#' Missouri's proportion of ED visits ensemble appears
#' through reference date `2026-03-28` and stops from
#' `2026-04-04`, which is the first reference date after
#' the minimum was added to
#' [write_ref_date_summary_all()].
#'
#' @param reference_date character or Date, the reference
#' date to look up.
#'
#' @return The minimum, as an integer.
#' @export
n_models_for_ens_reporting_as_of <- function(reference_date) {
  reference_date <- lubridate::as_date(reference_date)

  in_force <- ensemble_reporting_minimums |>
    dplyr::filter(.data$reference_date <= !!reference_date)

  if (nrow(in_force) == 0) {
    cli::cli_abort(
      c(
        "No ensemble reporting minimum recorded for
         {.val {as.character(reference_date)}}.",
        "i" = "The record starts at
               {.val {as.character(min(ensemble_reporting_minimums$reference_date))}}."
      )
    )
  }

  return(dplyr::last(in_force$n_models))
}
