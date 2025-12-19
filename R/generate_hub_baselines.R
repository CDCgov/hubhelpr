#' Checks latency of input data.
#'
#' @param data Data frame containing target data.
#' @param location_col_name Character. Name of the location column.
#' Default is "geo_value".
#' @param date_col_name Character. Name of the date column.
#' Default is "time_value".
#' @param target_label Character. Human-readable label for the target.
#' @param reference_date Date. The reference date for forecast.
#' Default is the current MMWR epiweek ceiling.
#' @param desired_max_time_value Date. The most recent date for which
#' observations are expected. Default is one week before the reference_date.
#' @param overlatent_err_thresh Numeric. Proportion threshold for raising error
#' vs warning. Default 0.20.
#'
#' @return Invisible NULL. Raises warnings or errors
#' based on the proportion of locations with excess latency.
#' @export
check_data_latency <- function(
  data,
  location_col_name = "geo_value",
  date_col_name = "time_value",
  target_label,
  reference_date = NULL,
  desired_max_time_value = NULL,
  overlatent_err_thresh = 0.20
) {
  if (is.null(reference_date)) {
    reference_date <- forecasttools::ceiling_mmwr_epiweek(lubridate::today())
  }
  if (is.null(desired_max_time_value)) {
    desired_max_time_value <- reference_date - lubridate::dweeks(1)
  }

  latency_tbl <- data |>
    dplyr::group_by(.data[[location_col_name]]) |>
    dplyr::summarise(
      latest_date = max(.data[[date_col_name]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      excess_latency = pmax(
        as.integer(desired_max_time_value - .data$latest_date) %/% 7L,
        0L
      ),
      has_excess_latency = .data$excess_latency > 0L
    )

  prop_locs_overlatent <- mean(latency_tbl$has_excess_latency)

  if (prop_locs_overlatent > overlatent_err_thresh) {
    cli::cli_abort(
      c(
        "{target_label}: More than {100 * overlatent_err_thresh}% of locations have excess latency.",
        "!" = "Reference date: {reference_date}",
        "!" = "Expected observations through: {desired_max_time_value}",
        "!" = "{nrow(latency_tbl |> dplyr::filter(.data$has_excess_latency))} location{?s} have excess latency"
      )
    )
  } else if (prop_locs_overlatent > 0) {
    cli::cli_warn(
      c(
        "{target_label}: Some locations have excess latency.",
        "!" = "Reference date: {reference_date}",
        "!" = "Expected observations through: {desired_max_time_value}",
        "!" = "{nrow(latency_tbl |> dplyr::filter(.data$has_excess_latency))} location{?s} have excess latency"
      )
    )
  }
  return(invisible())
}


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
#'  Function will error if there is excess latency; see [check_data_latency()].
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

  check_data_latency(
    data = epi_df,
    reference_date = reference_date,
    desired_max_time_value = desired_max_time_value,
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


#' Generate hub baseline forecasts for a given disease and reference date
#'
#' @param base_hub_path Path to the base hub directory.
#' @param reference_date Reference date (should be a Saturday).
#' @param disease Disease name ("covid" or "rsv").
#' @param as_of As of date to filter to, as an object
#' coercible by as.Date(), or "latest" to filter to the
#' most recent available vintage. Default "latest".
#' @return NULL. Writes baseline forecast file to hub's model-output directory.
#' @export
generate_hub_baseline <- function(
  base_hub_path,
  reference_date,
  disease,
  as_of = "latest"
) {
  checkmate::assert_scalar(disease)
  checkmate::assert_names(disease, subset.of = c("covid", "rsv"))
  reference_date <- lubridate::as_date(reference_date)
  desired_max_time_value <- reference_date - 7L
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

  baseline_model_name <- glue::glue("{get_hub_name(disease)}-baseline")
  output_dirpath <- fs::path(base_hub_path, "model-output", baseline_model_name)
  if (!fs::dir_exists(output_dirpath)) {
    fs::dir_create(output_dirpath, recurse = TRUE)
  }

  hub_target_data <- hubData::connect_target_timeseries(base_hub_path) |>
    dplyr::collect() |>
    forecasttools::hub_target_data_as_of(as_of)

  preds_hosp <- make_baseline_forecast(
    target_data = hub_target_data,
    target_name = glue::glue("wk inc {disease} hosp"),
    target_label = "Hospital Admissions",
    reference_date = reference_date,
    desired_max_time_value = desired_max_time_value,
    rng_seed = rng_seed
  )

  preds_ed <- make_baseline_forecast(
    target_data = hub_target_data,
    target_name = glue::glue("wk inc {disease} prop ed visits"),
    target_label = "Proportion ED Visits",
    reference_date = reference_date,
    desired_max_time_value = desired_max_time_value,
    rng_seed = rng_seed
  )

  forecasttools::write_tabular_file(
    dplyr::bind_rows(preds_hosp, preds_ed),
    fs::path(
      output_dirpath,
      glue::glue("{reference_date}-{baseline_model_name}"),
      ext = "csv"
    )
  )

  return(invisible())
}
