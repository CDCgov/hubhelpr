#' Summarize forecast hub data for a specific reference date.
#'
#' This function generates a tibble of forecast data
#' for a given reference date. It can filter by model IDs;
#' allows flexibility retrieve all models or specific
#' subsets (e.g., ensemble only).
#'
#' @param reference_date character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param base_hub_path character, path to the forecast hub
#' directory.
#' @param disease character, disease name ("covid" or
#' "rsv"). Used to derive hub name and file prefix.
#' @param population_data data frame with columns "location"
#' and "population". Adds population-based calculations.
#' @param horizons_to_include integer vector, horizons to
#' include in the output. Default: c(0, 1, 2).
#' @param excluded_locations character vector of location
#' codes to exclude from the output. Default: character(0).
#' @param targets character vector, target name(s) to filter
#' forecasts. If NULL (default), does not filter by target.
#' @param model_ids character vector of model IDs to include.
#' If NULL (default), includes all models.
#'
#' @return tibble containing forecast summary data
#'
#' @export
summarize_ref_date_forecasts <- function(
  reference_date,
  base_hub_path,
  disease,
  population_data,
  horizons_to_include = c(0, 1, 2),
  excluded_locations = character(0),
  targets = NULL,
  model_ids = NULL
) {
  reference_date <- lubridate::as_date(reference_date)

  model_metadata <- hubData::load_model_metadata(
    base_hub_path,
    model_ids = model_ids
  )

  hub_content <- hubData::connect_hub(base_hub_path)

  current_forecasts <- hub_content |>
    dplyr::filter(
      .data$reference_date == !!reference_date,
      !(.data$location %in% !!excluded_locations),
      .data$horizon %in% !!horizons_to_include
    ) |>
    hubData::collect_hub() |>
    dplyr::filter(
      forecasttools::nullable_comparison(.data$target, "%in%", !!targets),
      forecasttools::nullable_comparison(.data$model_id, "%in%", !!model_ids)
    )

  if (nrow(current_forecasts) == 0) {
    model_filter_msg <- if (!is.null(model_ids)) {
      glue::glue(" and model IDs: {paste(model_ids, collapse = ', ')}")
    } else {
      ""
    }
    cli::cli_abort(
      glue::glue(
        "No forecast data found for reference date {reference_date}",
        "{model_filter_msg}"
      )
    )
  }

  forecasts_data <- forecasttools::pivot_hubverse_quantiles_wider(
    hubverse_table = current_forecasts,
    pivot_quantiles = c(
      "quantile_0.025" = 0.025,
      "quantile_0.25" = 0.25,
      "quantile_0.5" = 0.5,
      "quantile_0.75" = 0.75,
      "quantile_0.975" = 0.975
    )
  ) |>
    dplyr::mutate(
      location_name = forecasttools::us_location_recode(
        .data$location,
        "hub",
        "name"
      ),
      abbreviation = forecasttools::us_location_recode(
        .data$location,
        "hub",
        "abbr"
      )
    ) |>
    dplyr::mutate(
      location_name = dplyr::case_match(
        .data$location_name,
        "United States" ~ "US",
        .default = .data$location_name
      )
    ) |>
    dplyr::left_join(
      population_data,
      by = c("location_name" = "location")
    ) |>
    dplyr::mutate(
      population = as.numeric(.data$population),
      quantile_0.025_per100k = .data$quantile_0.025 / .data$population * 100000,
      quantile_0.5_per100k = .data$quantile_0.5 / .data$population * 100000,
      quantile_0.975_per100k = .data$quantile_0.975 / .data$population * 100000,
      quantile_0.025_count = .data$quantile_0.025,
      quantile_0.5_count = .data$quantile_0.5,
      quantile_0.975_count = .data$quantile_0.975
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("quantile_"),
        round,
        .names = "{.col}_rounded"
      ),
      forecast_due_date = as.Date(!!reference_date) - 3,
      location_sort_order = ifelse(.data$location_name == "US", 0, 1)
      # order table with national first, then alphabetically
    ) |>
    dplyr::arrange(.data$location_sort_order, .data$location_name) |>
    dplyr::left_join(
      dplyr::distinct(
        model_metadata,
        .data$model_id,
        .keep_all = TRUE
      ),
      by = "model_id"
    ) |>
    dplyr::mutate(
      reference_date = as.Date(.data$reference_date),
      target_end_date = as.Date(.data$target_end_date),
      target_end_date_formatted = format(.data$target_end_date, "%B %d, %Y"),
      reference_date_formatted = format(.data$reference_date, "%B %d, %Y"),
      forecast_due_date_formatted = format(.data$forecast_due_date, "%B %d, %Y")
    )

  return(forecasts_data)
}
