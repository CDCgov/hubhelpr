task_id_cols <- c(
  "reference_date",
  "location",
  "horizon",
  "target",
  "target_end_date"
)


#' Create an ensemble forecast for a single target
#'
#' @param weekly_models Data frame of model metadata for the week.
#' Must include `model_id`, `designated_model` and `target` columns.
#' @param weekly_forecasts Data frame of forecasts for the week.
#' @param target_name Character. Name of the target to ensemble,
#' e.g., "wk inc covid hosp".
#' @param ensemble_model_id Character. Model_id to assign to the ensemble output.
#' @param ensemble_output_type Output type to ensemble. Default "quantile".
#' @param ensemble_agg_fun Aggregation function to use. Default "median".
#' @return A data frame of ensemble forecasts for the specified target.
ensemble_by_target <- function(
  weekly_models,
  weekly_forecasts,
  target_name,
  ensemble_model_id,
  ensemble_output_type = "quantile",
  ensemble_agg_fun = "median"
) {
  checkmate::assert_names(
    colnames(weekly_models),
    must.include = c("model_id", "designated_model", "target"),
    .var.name = "weekly_models columns"
  )

  expected_cols <- c(names(forecasttools::hubverse_std_colnames), task_id_cols)
  checkmate::assertSetEqual(
    colnames(weekly_forecasts),
    expected_cols,
    .var.name = "weekly_forecasts columns"
  )

  if (ensemble_output_type != "quantile") {
    stop("Only 'quantile' ensemble_output_type is currently supported")
  }

  eligible_models <- weekly_models |>
    dplyr::filter(.data$designated_model, .data$target == !!target_name)

  forecasts_to_ensemble <- weekly_forecasts |>
    dplyr::filter(
      model_id %in% eligible_models$model_id,
      output_type == !!ensemble_output_type,
      target == !!target_name
    ) |>
    # ensure quantiles are handled accurately even with leading/trailing zeros
    dplyr::mutate(output_type_id = as.factor(as.numeric(output_type_id)))

  median_ensemble_outputs <- forecasts_to_ensemble |>
    hubEnsembles::simple_ensemble(
      agg_fun = ensemble_agg_fun,
      model_id = ensemble_model_id,
      task_id_cols = task_id_cols
    ) |>
    dplyr::mutate(value = pmax(value, 0)) |>
    dplyr::select(-"model_id")

  return(median_ensemble_outputs)
}


#' Generate hub ensemble forecasts for a given disease and reference date
#'
#' @param base_hub_path Path to the base hub directory.
#' @param reference_date Reference date (should be a Saturday).
#' @param disease Disease name ("covid" or "rsv").
#' @param ensemble_targets A vector specifying targets to generate ensemble
#' forecasts for, e.g., c("hosp", "prop ed visits"). Defaults to "hosp".
#' @return NULL. Writes ensemble forecast file to hub's model-output directory.
#' @export
generate_hub_ensemble <- function(
  base_hub_path,
  reference_date,
  disease,
  ensemble_targets = c("hosp")
) {
  if (!disease %in% c("covid", "rsv")) {
    stop("'disease' must be either 'covid' or 'rsv'")
  }
  reference_date <- lubridate::as_date(reference_date)

  dow_supplied <- lubridate::wday(reference_date, week_start = 7, label = FALSE)
  if (dow_supplied != 7) {
    cli::cli_abort(
      message = paste0(
        "Expected `reference_date` to be a Saturday, day number 7 ",
        "of the week, given the `week_start` value of Sunday. ",
        "Got {reference_date}, which is day number ",
        "{dow_supplied} of the week."
      )
    )
  }

  ensemble_model_name <- ensemble_model_names[[disease]]
  hub_name <- stringr::str_split_i(ensemble_model_name, "-", 1)

  output_dirpath <- fs::path(base_hub_path, "model-output", ensemble_model_name)
  output_filename <- paste0(
    as.character(reference_date),
    "-",
    hub_name,
    "-ensemble.csv"
  )

  if (!fs::dir_exists(output_dirpath)) {
    fs::dir_create(output_dirpath, recurse = TRUE)
  }

  weekly_forecasts <- hubData::connect_hub(base_hub_path) |>
    dplyr::filter(
      .data$reference_date == !!reference_date,
      !stringr::str_detect(.data$model_id, hub_name)
    ) |>
    hubData::collect_hub()

  weekly_models <- hubData::load_model_metadata(
    base_hub_path,
    model_ids = unique(weekly_forecasts$model_id)
  ) |>
    dplyr::select("model_id", "designated_model") |>
    dplyr::distinct() |>
    dplyr::right_join(
      weekly_forecasts |>
        dplyr::select("model_id", "target") |>
        dplyr::distinct(),
      by = "model_id"
    ) |>
    dplyr::arrange(.data$target)

  forecasttools::write_tabular(
    weekly_models,
    fs::path(
      base_hub_path,
      "auxiliary-data",
      "weekly-model-submissions",
      glue::glue("{reference_date}-models-submitted-to-hub.csv")
    )
  )

  median_ensemble_outputs <- purrr::map(
    ensemble_targets,
    function(ensemble_target) {
      ensemble_by_target(
        weekly_models,
        weekly_forecasts,
        target_name = glue::glue("wk inc {disease} {ensemble_target}"),
        ensemble_model_id = paste0(hub_name, "-quantile-median-ensemble")
      )
    }
  ) |>
    dplyr::bind_rows()

  forecasttools::write_tabular(
    median_ensemble_outputs,
    fs::path(output_dirpath, output_filename)
  )
  invisible()
}
