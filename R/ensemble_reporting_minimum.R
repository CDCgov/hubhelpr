#' Path to a hub's record of ensemble reporting
#' minimums. Record cntained in `config/<hub>`.
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
ens_min_designated_models_file_path <- function(
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


#' Parse a hub's ensemble reporting minimums file.
#'
#' The file is TOML and keyed by the reference date
#' each minimum designated model count was
#' instantiated. E.g.:
#'
#' ```toml
#' 2024-11-23 = 0
#' 2026-04-04 = 2
#' ```
#'
#' @param path character, path to the minimums file.
#'
#' @return A tibble with columns `reference_date` and
#' `n_models`, ordered by `reference_date`.
#' @export
parse_ens_min_designated_models_file <- function(path) {
  if (!fs::file_exists(path)) {
    cli::cli_abort(
      c(
        "No ensemble reporting minimums file at {.path {path}}.",
        "i" = "Each hub has a record file, with the minimum designated models required from
               each reference date onward."
      )
    )
  }

  minimums <- RcppTOML::parseTOML(path) |>
    tibble::enframe(name = "reference_date_raw", value = "n_models") |>
    tidyr::unnest("n_models") |>
    dplyr::mutate(
      reference_date = lubridate::ymd(.data$reference_date_raw, quiet = TRUE)
    )

  malformed_dates <- minimums |>
    dplyr::filter(is.na(.data$reference_date)) |>
    dplyr::pull("reference_date_raw")

  if (length(malformed_dates) > 0) {
    cli::cli_abort(
      c(
        "Every key in {.path {path}} must be a reference date in
         YYYY-MM-DD format.",
        "x" = "Found: {.val {malformed_dates}}."
      )
    )
  }

  checkmate::assert_integerish(
    minimums$n_models,
    lower = 0,
    any.missing = FALSE,
    .var.name = glue::glue("minimums in {path}")
  )

  return(
    minimums |>
      dplyr::mutate(n_models = as.integer(.data$n_models)) |>
      dplyr::arrange(.data$reference_date) |>
      dplyr::select("reference_date", "n_models")
  )
}


#' Get a hub's ensemble reporting minimum designated
#' models for one reference date.
#'
#' @param hub_reports_path character, path to the
#' forecast hub reports directory.
#' @param disease character, disease name ("covid" or
#' "rsv").
#' @param reference_date character or Date, the
#' reference date to look up.
#'
#' @return The minimum, as an integer.
#' @export
get_reference_date_ens_minimum <- function(
  hub_reports_path,
  disease,
  reference_date
) {
  reference_date <- lubridate::as_date(reference_date)

  minimums <- parse_ens_min_designated_models_file(
    ens_min_designated_models_file_path(hub_reports_path, disease)
  )

  required_minimum <- minimums |>
    dplyr::filter(.data$reference_date <= !!reference_date)

  if (nrow(required_minimum) == 0) {
    cli::cli_abort(
      c(
        "No ensemble reporting minimum recorded for
         {.val {as.character(reference_date)}}.",
        "i" = "The record starts at
               {.val {as.character(min(minimums$reference_date))}}."
      )
    )
  }

  return(dplyr::last(required_minimum$n_models))
}
