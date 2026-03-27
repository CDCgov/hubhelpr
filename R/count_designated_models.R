#' Count designated models per reference_date, target,
#' location, and horizon.
#'
#' Gets the number of designated models that
#' contributed forecasts for each combination of target,
#' location, reference date, and horizon; useful for
#' checking minimum model thresholds for ensemble
#' reporting.
#'
#' @param base_hub_path character, path to the base hub
#' directory.
#' @param reference_dates character or Date vector of
#' reference dates in YYYY-MM-DD format. If NULL
#' (default), includes all available reference dates.
#' @param targets character vector of target names to
#' include. If NULL (default), includes all supported
#' targets.
#' @param horizons integer vector of horizons to include.
#' If NULL (default), includes all available horizons.
#' @param output_types character vector of output types
#' to include. Default: "quantile".
#'
#' @return A tibble with columns `reference_date`,
#' `target`, `location`, `horizon`, `output_type`,
#' and `n_models`.
#'
#' @export
count_designated_models <- function(
  base_hub_path,
  reference_dates = NULL,
  targets = NULL,
  horizons = NULL,
  output_types = "quantile"
) {
  hub_forecasts <- hubData::connect_hub(base_hub_path)

  if (!is.null(reference_dates)) {
    reference_dates <- lubridate::as_date(reference_dates)
    hub_forecasts <- hub_forecasts |>
      dplyr::filter(.data$reference_date %in% !!reference_dates)
  }

  hub_forecasts <- hubData::collect_hub(hub_forecasts)

  if (nrow(hub_forecasts) == 0) {
    cli::cli_abort(
      "No forecast data found for the specified reference date(s)."
    )
  }

  hub_submitting_models <- hubData::load_model_metadata(
    base_hub_path,
    model_ids = unique(hub_forecasts$model_id)
  ) |>
    dplyr::select("model_id", "designated_model") |>
    dplyr::distinct()

  designated_ids <- hub_submitting_models |>
    dplyr::filter(.data$designated_model) |>
    dplyr::pull("model_id")

  designated_forecasts <- hub_forecasts |>
    dplyr::filter(
      .data$model_id %in% designated_ids,
      forecasttools::nullable_comparison(.data$target, "%in%", !!targets),
      forecasttools::nullable_comparison(.data$horizon, "%in%", !!horizons),
      forecasttools::nullable_comparison(
        .data$output_type,
        "%in%",
        !!output_types
      )
    )

  hub_task_grid <- get_hub_tasks(base_hub_path) |>
    dplyr::distinct(
      .data$reference_date,
      .data$target,
      .data$location,
      .data$horizon,
      .data$output_type
    ) |>
    dplyr::filter(
      forecasttools::nullable_comparison(
        .data$reference_date,
        "%in%",
        !!reference_dates
      ),
      forecasttools::nullable_comparison(.data$target, "%in%", !!targets),
      forecasttools::nullable_comparison(.data$horizon, "%in%", !!horizons),
      forecasttools::nullable_comparison(
        .data$output_type,
        "%in%",
        !!output_types
      )
    )

  designated_counts <- designated_forecasts |>
    dplyr::summarise(
      n_models = dplyr::n_distinct(.data$model_id),
      .by = c("reference_date", "target", "location", "horizon", "output_type")
    )

  model_counts <- hub_task_grid |>
    dplyr::left_join(
      designated_counts,
      by = c("reference_date", "target", "location", "horizon", "output_type")
    ) |>
    dplyr::mutate(
      n_models = dplyr::coalesce(.data$n_models, 0L)
    )

  return(model_counts)
}
