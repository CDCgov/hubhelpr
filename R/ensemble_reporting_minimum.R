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


#' Path to a hub's record of the ensemble reporting
#' minimums that were applied. Contained in
#' `config/<hub>`.
#'
#' @param hub_reports_path character, path to the
#' forecast hub reports directory.
#' @param disease character, disease name ("covid" or
#' "rsv").
#' @param directory character, directory under
#' `hub_reports_path` holding per-hub configuration.
#' Default: "config".
#' @param filename character, name of the record within a
#' hub's configuration directory. Default:
#' "ensemble_reporting_minimums.toml".
#'
#' @return The file path, whether or not it exists.
#' @noRd
applied_minimums_path <- function(
  hub_reports_path,
  disease,
  directory = "config",
  filename = "ensemble_reporting_minimums.toml"
) {
  return(fs::path(
    hub_reports_path,
    directory,
    get_hub_repo_name(disease),
    filename
  ))
}


#' Record the ensemble reporting minimum desingated
#' models applied to a reference date.
#'
#' @inheritParams applied_minimums_path
#' @param reference_date character or Date, the
#' reference date generated.
#' @param n_models integer, the minimum applied.
#'
#' @return Invisibly, the path written to.
#' @export
record_applied_ensemble_minimum <- function(
  hub_reports_path,
  disease,
  reference_date,
  n_models
) {
  reference_date <- as.character(lubridate::as_date(reference_date))
  n_models <- as.integer(n_models)
  path <- applied_minimums_path(hub_reports_path, disease)

  recorded <- if (fs::file_exists(path)) {
    RcppTOML::parseTOML(path)
  } else {
    list()
  }

  previous <- recorded[[reference_date]]

  if (!is.null(previous) && as.integer(previous) != n_models) {
    cli::cli_abort(
      c(
        "Reference date {.val {reference_date}} was generated with an
         ensemble reporting minimum of {.val {as.integer(previous)}},
         but {.val {n_models}} was applied now.",
        "i" = "Regenerating a week under a different rule than it was
               published under changes what the report says without
               changing any forecast. Append a row to
               {.code ensemble_reporting_minimums} if the rule actually
               changed."
      )
    )
  }

  recorded[[reference_date]] <- n_models

  fs::dir_create(fs::path_dir(path))
  writeLines(
    c(
      "# Ensemble reporting minimums actually applied when generating",
      "# this hub's weekly summaries.",
      "#",
      "# Written by hubhelpr at generation time. The rule itself lives",
      "# in hubhelpr's `ensemble_reporting_minimums`; this records what",
      "# was used",
      "",
      purrr::imap_chr(
        recorded[order(names(recorded))],
        \(value, key) glue::glue("{key} = {as.integer(value)}")
      )
    ),
    path
  )

  return(invisible(path))
}


#' Resolve the ensemble reporting minimum for a
#' reference date, with a warning if an override
#' contradicts the record.
#'
#' @param n_models_for_ens_reporting integer or NULL,
#' the value supplied.
#' @param reference_date character or Date, the
#' reference date being generated.
#'
#' @return The minimum to apply, as an integer.
#' @noRd
resolve_ens_reporting_minimum <- function(
  n_models_for_ens_reporting,
  reference_date
) {
  recorded <- n_models_for_ens_reporting_as_of(reference_date)

  if (is.null(n_models_for_ens_reporting)) {
    return(recorded)
  }

  n_models_for_ens_reporting <- as.integer(n_models_for_ens_reporting)

  if (n_models_for_ens_reporting != recorded) {
    cli::cli_warn(
      c(
        "Using an ensemble reporting minimum of
         {.val {n_models_for_ens_reporting}} for
         {.val {as.character(lubridate::as_date(reference_date))}}, but
         the record has {.val {recorded}}.",
        "i" = "Append a row to {.code ensemble_reporting_minimums} if the
               rule has changed, so that regenerating this week later
               applies the same minimum."
      )
    )
  }

  return(n_models_for_ens_reporting)
}
