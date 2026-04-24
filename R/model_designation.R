#' Resolve per-target model designation.
#'
#' Returns full grid of (model_id, target,
#' designated) for the requested models and targets.
#' Resolution uses the model's metadata fields
#' `designated_model` and the optional
#' `designated_targets` list. If `designated_model`
#' is FALSE: never designated. If `designated_model`
#' is TRUE and `designated_targets` is absent or
#' empty: designated for every target. If
#' `designated_model` is TRUE and `designated_targets`
#' is present: designated only for listed targets.
#'
#' @param base_hub_path character, path to the base hub
#' directory.
#' @param model_ids character vector of model IDs to
#' include, or NULL (default) to include all models
#' with metadata in the hub.
#' @param targets character vector of target names to
#' include, or NULL (default) to include all targets
#' supported by the hub.
#'
#' @return A tibble with columns `model_id`, `target`,
#' and `designated` (logical), with one row per
#' (model, target) combination.
#' @export
get_model_designation <- function(
  base_hub_path,
  model_ids = NULL,
  targets = NULL
) {
  if (is.null(targets)) {
    targets <- get_hub_supported_targets(base_hub_path)
  }

  metadata <- hubData::load_model_metadata(
    base_hub_path,
    model_ids = model_ids
  ) |>
    dplyr::select(
      "model_id",
      "designated_model",
      dplyr::any_of("designated_targets")
    )

  if (!"designated_targets" %in% colnames(metadata)) {
    metadata <- metadata |>
      dplyr::mutate(designated_targets = list(targets))
  } else {
    metadata <- metadata |>
      dplyr::mutate(
        designated_targets = purrr::map(
          .data$designated_targets,
          ~ if (length(.x) == 0L || anyNA(.x)) targets else as.character(.x)
        )
      )
  }

  metadata |>
    tidyr::crossing(target = targets) |>
    dplyr::mutate(
      designated = .data$designated_model &
        purrr::map2_lgl(
          .data$target,
          .data$designated_targets,
          `%in%`
        )
    ) |>
    dplyr::select("model_id", "target", "designated")
}


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
#' to include. Default: NULL (include all available
#' output types).
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
  output_types = NULL
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

  designated_pairs <- get_model_designation(
    base_hub_path,
    model_ids = dplyr::distinct(hub_forecasts, .data$model_id) |>
      dplyr::pull(.data$model_id)
  ) |>
    dplyr::filter(.data$designated) |>
    dplyr::select("model_id", "target")

  designated_forecasts <- hub_forecasts |>
    dplyr::inner_join(designated_pairs, by = c("model_id", "target")) |>
    dplyr::filter(
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

  hub_task_grid |>
    dplyr::left_join(
      designated_counts,
      by = c("reference_date", "target", "location", "horizon", "output_type")
    ) |>
    dplyr::mutate(
      n_models = dplyr::coalesce(.data$n_models, 0L)
    )
}
