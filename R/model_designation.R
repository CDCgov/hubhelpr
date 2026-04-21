#' Resolve per-target model designation.
#'
#' Returns full grid of (model_id, target,
#' designated_model) for the requested models and
#' targets. Resolution uses the model's metadata
#' fields `designated_model` and the optional
#' `designated_targets` list. If `designated_model`
#' absent or FALSE: never designated. If
#' `designated_model` TRUE and `designated_targets`
#' absent or empty: designated for every target. If
#' `designated_model` TRUE and `designated_targets`
#' present: designated only for targets in that list.
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
#' and `designated_model` (logical), with one row per
#' (model, target) combination.
#' @export
get_model_designation <- function(
  base_hub_path,
  model_ids = NULL,
  targets = NULL
) {
  metadata <- hubData::load_model_metadata(
    base_hub_path,
    model_ids = model_ids
  ) |>
    dplyr::select(
      "model_id",
      "designated_model",
      dplyr::any_of("designated_targets")
    ) |>
    dplyr::distinct()

  if (!"designated_targets" %in% colnames(metadata)) {
    metadata <- metadata |>
      dplyr::mutate(designated_targets = NA_character_)
  } else {
    metadata <- metadata |>
      dplyr::mutate(
        designated_targets = as.character(.data$designated_targets)
      )
  }

  if (is.null(targets)) {
    targets <- get_hub_supported_targets(base_hub_path)
  }

  model_target_grid <- tidyr::crossing(
    model_id = unique(metadata$model_id),
    target = targets
  )

  designated_all_targets <- metadata |>
    dplyr::filter(.data$designated_model, is.na(.data$designated_targets)) |>
    dplyr::distinct(.data$model_id)

  designated_specific_targets <- metadata |>
    dplyr::filter(.data$designated_model, !is.na(.data$designated_targets)) |>
    dplyr::distinct(.data$model_id, .data$designated_targets) |>
    dplyr::rename(target = "designated_targets")

  designated_pairs <- dplyr::bind_rows(
    designated_all_targets |>
      tidyr::crossing(target = targets),
    designated_specific_targets
  ) |>
    dplyr::mutate(designated_model = TRUE)

  model_target_grid |>
    dplyr::left_join(designated_pairs, by = c("model_id", "target")) |>
    dplyr::mutate(
      designated_model = dplyr::coalesce(.data$designated_model, FALSE)
    )
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
    model_ids = unique(hub_forecasts$model_id)
  ) |>
    dplyr::filter(.data$designated_model) |>
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
