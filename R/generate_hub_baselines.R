#' Create a hub formatted baseline forecast for a single target
#'
#' @param target_data Data frame of target time series. Must include `date`,
#' `location`, `target`, and `observation` columns.
#' @param target_name Character. Name of the target to forecast,
#' e.g., "wk inc covid hosp".
#' @param target_label Character. Label for the target, e.g., "Hospital Admissions"
#' or "Proportion ED Visits".
#' @param reference_date Date. Reference date for the forecast.
#' @param desired_max_time_value Date. Most recent date for which observations are expected.
#'  Function will error if there is excess latency; see [assert_data_up_to_date()].
#' @param rng_seed Integer. Random seed for reproducibility.
#' @return A data frame of baseline forecasts for the specified target.
make_baseline_forecast <- function(
  target_data,
  target_name,
  target_label,
  reference_date,
  desired_max_time_value,
  rng_seed
) {
  checkmate::assertSubset(
    c("date", "location", "target", "observation"),
    colnames(target_data),
    .var.name = "target_data columns"
  )

  epi_df <- target_data |>
    dplyr::filter(.data$target == !!target_name) |>
    dplyr::mutate(
      geo_value = forecasttools::us_location_recode(
        .data$location,
        "code",
        "abbr"
      )
    ) |>
    dplyr::rename(
      time_value = "date"
    ) |>
    dplyr::select(-c("location", "target")) |>
    epiprocess::as_epi_df()

  assert_data_up_to_date(
    data = epi_df,
    expected_max_time_value = desired_max_time_value,
    target_label = target_label
  )

  args_list <- epipredict::cdc_baseline_args_list(aheads = 1:4, nsims = 1e5)
  preds <- withr::with_rng_version(
    "4.0.0",
    withr::with_seed(rng_seed, {
      fcst <- epipredict::cdc_baseline_forecaster(
        epi_df |>
          dplyr::filter(
            .data$time_value <= desired_max_time_value
          ),
        "observation",
        args_list
      )
      # advance forecast_date by a week due to data latency and
      # create forecast for horizon -1
      fcst$predictions |>
        dplyr::mutate(
          forecast_date = reference_date,
          ahead = as.integer(.data$target_date - reference_date) %/% 7L
        ) |>
        # prepare -1 horizon predictions
        dplyr::bind_rows(
          epi_df |>
            tidyr::drop_na(.data$observation) |>
            dplyr::slice_max(.data$time_value) |>
            dplyr::transmute(
              forecast_date = reference_date,
              target_date = reference_date - 7L,
              ahead = -1L,
              geo_value,
              .pred = .data$observation,
              .pred_distn = hardhat::quantile_pred(
                values = matrix(
                  rep(
                    .data$observation,
                    each = length(args_list$quantile_levels)
                  ),
                  nrow = length(.data$observation),
                  ncol = length(args_list$quantile_levels),
                  byrow = TRUE
                ),
                quantile_levels = args_list$quantile_levels
              )
            )
        )
    })
  )

  preds_formatted <- preds |>
    epipredict::flusight_hub_formatter(
      target = target_name,
      output_type = "quantile"
    ) |>
    tidyr::drop_na(.data$output_type_id) |>
    dplyr::arrange(.data$target, .data$horizon, .data$location) |>
    dplyr::select(
      "reference_date",
      "horizon",
      "target",
      "target_end_date",
      "location",
      "output_type",
      "output_type_id",
      "value"
    )
  return(preds_formatted)
}


#' Generate hub baseline forecasts for a given disease and
#' reference date.
#'
#' @param base_hub_path Path to the base hub directory.
#' @param reference_date Reference date (should be a Saturday).
#' @param disease Disease name ("covid" or "rsv").
#' @param targets Character vector of full target names to
#' generate baselines for (e.g., c("wk inc covid hosp",
#' "wk inc covid prop ed visits")). Defaults to NULL,
#' which generates baselines for all unique targets in
#' the time-series data.
#' @param as_of As of date to filter to, as an object
#' coercible by as.Date(), or "latest" to filter to the
#' most recent available vintage. Default "latest".
#' @param output_format Character, output file format. One
#' of "csv", "tsv", or "parquet". Default: "csv".
#' @return NULL. Writes baseline forecast file to hub's
#' model-output directory.
#' @export
generate_hub_baseline <- function(
  base_hub_path,
  reference_date,
  disease,
  targets = NULL,
  as_of = "latest",
  output_format = "csv"
) {
  checkmate::assert_scalar(disease)
  checkmate::assert_names(disease, subset.of = c("covid", "rsv"))
  reference_date <- lubridate::as_date(reference_date)
  desired_max_time_value <- reference_date - lubridate::weeks(1)
  dow_supplied <- lubridate::wday(reference_date, week_start = 7, label = FALSE)
  rng_seed <- as.integer((59460707 + as.numeric(reference_date)) %% 2e9)
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

  available_targets <- get_unique_targets(base_hub_path, disease)

  if (is.null(targets)) {
    targets <- available_targets
  } else {
    invalid_targets <- setdiff(targets, available_targets)
    if (length(invalid_targets) > 0) {
      cli::cli_abort(
        c(
          "Requested targets not found in time-series data:",
          "x" = "Invalid targets: {.val {invalid_targets}}",
          "i" = "Available targets: {.val {available_targets}}"
        )
      )
    }
  }

  baseline_model_name <- glue::glue("{get_hub_name(disease)}-baseline")
  output_dirpath <- fs::path(base_hub_path, "model-output", baseline_model_name)
  if (!fs::dir_exists(output_dirpath)) {
    fs::dir_create(output_dirpath, recurse = TRUE)
  }

  hub_target_data <- hubData::connect_target_timeseries(base_hub_path) |>
    dplyr::collect() |>
    forecasttools::hub_target_data_as_of(as_of)

  all_preds <- purrr::map(targets, function(target_name) {
    target_label <- get_target_label(target_name)
    make_baseline_forecast(
      target_data = hub_target_data,
      target_name = target_name,
      target_label = target_label,
      reference_date = reference_date,
      desired_max_time_value = desired_max_time_value,
      rng_seed = rng_seed
    )
  }) |>
    dplyr::bind_rows()

  forecasttools::write_tabular_file(
    all_preds,
    fs::path(
      output_dirpath,
      glue::glue("{reference_date}-{baseline_model_name}"),
      ext = output_format
    )
  )

  return(invisible())
}
