#' Generate map data file containing ensemble forecast
#' data.
#'
#' This function loads the latest ensemble forecast data
#' from the forecast hub and processes it into the required
#' format. The resulting file contains forecast values for
#' all regions (including US, DC, and Puerto Rico), for
#' various forecast horizons, and quantiles (0.025, 0.5,
#' and 0.975).
#'
#' The ensemble data is expected to contain the following
#' columns:
#' - `reference_date`: the date of the forecast
#' - `location`: state abbreviation
#' - `horizon`: forecast horizon
#' - `target`: forecast target (e.g., "wk inc covid hosp")
#' - `target_end_date`: the forecast target date
#' - `output_type`: type of output (e.g., "quantile")
#' - `output_type_id`: quantile value (e.g., 0.025, 0.5,
#'    0.975)
#' - `value`: forecast value
#'
#' The resulting map file will have the following columns:
#' - `location_name`: full state name (including "US" for
#'    the US state)
#' - `quantile_*`: the quantile forecast values (rounded
#'    to two decimal places)
#' - `horizon`: forecast horizon
#' - `target`: forecast target (e.g., "7 day ahead inc
#'    hosp")
#' - `target_end_date`: target date for the forecast (Ex:
#'    2024-11-30)
#' - `reference_date`: date that the forecast was generated
#'    (Ex: 2024-11-23)
#' - `target_end_date_formatted`: target date for the
#'    forecast, prettily re-formatted as a string (Ex:
#'    "November 30, 2024")
#' - `reference_date_formatted`: date that the forecast
#'    was generated, prettily re-formatted as a string
#'    (Ex: "November 23, 2024")
#'
#' @param reference_date character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param base_hub_path character, path to the forecast
#' hub directory.
#' @param hub_reports_path character, path to forecast hub
#' reports directory.
#' @param disease character, disease name ("covid" or "rsv").
#' Used to derive hub name and file prefix.
#' @param horizons_to_include integer vector, horizons to
#' include in the output. Default: c(0, 1, 2).
#' @param population_data data frame with columns
#' "location_name" and "population".
#' @param excluded_locations character vector of location
#' codes to exclude from the output. Default: character(0).
#' @param output_format character, output file format.
#' One of "csv", "tsv", or "parquet". Default: "csv".
#'
#' @export
get_map_data <- function(
  reference_date,
  base_hub_path,
  hub_reports_path,
  disease,
  horizons_to_include = c(0, 1, 2),
  population_data,
  excluded_locations = character(0),
  output_format = "csv"
) {
  checkmate::assert_scalar(disease)
  checkmate::assert_names(disease, subset.of = c("covid", "rsv"))
  checkmate::assert_subset(horizons_to_include, choices = c(-1, 0, 1, 2, 3))
  checkmate::assert_data_frame(population_data)
  checkmate::assert_names(
    colnames(population_data),
    must.include = c("location_name", "population")
  )
  checkmate::assert_character(excluded_locations)
  checkmate::assert_choice(output_format, choices = c("csv", "tsv", "parquet"))

  reference_date <- lubridate::as_date(reference_date)

  hub_name <- get_hub_name(disease)
  file_prefix <- disease

  # load the latest ensemble data from the model-output folder
  ensemble_model_name <- glue::glue("{hub_name}-ensemble")

  ensemble_data <- hubData::connect_hub(base_hub_path) |>
    dplyr::filter(
      .data$reference_date == !!reference_date,
      .data$model_id == !!ensemble_model_name
    ) |>
    hubData::collect_hub()

  if (nrow(ensemble_data) == 0) {
    cli::cli_abort(
      glue::glue(
        "No ensemble data found for reference date {reference_date} ",
        "and model {ensemble_model_name}"
      )
    )
  }

  # process ensemble data into the required format for Map file
  map_data <- forecasttools::pivot_hubverse_quantiles_wider(
    hubverse_table = ensemble_data,
    pivot_quantiles = c(
      "quantile_0.025" = 0.025,
      "quantile_0.25" = 0.25,
      "quantile_0.5" = 0.5,
      "quantile_0.75" = 0.75,
      "quantile_0.975" = 0.975
    )
  ) |>
    dplyr::filter(.data$horizon %in% !!horizons_to_include) |>
    dplyr::filter(!(.data$location %in% !!excluded_locations)) |>
    dplyr::mutate(
      reference_date = as.Date(.data$reference_date),
      target_end_date = as.Date(.data$target_end_date),
      model = !!ensemble_model_name
    ) |>
    # convert location column codes to full location names
    dplyr::mutate(
      location = forecasttools::us_location_recode(
        .data$location,
        "hub",
        "name"
      )
    ) |>
    # long name "United States" to "US"
    dplyr::mutate(
      location = dplyr::case_match(
        .data$location,
        "United States" ~ "US",
        .default = .data$location
      ),
      # sort locations alphabetically, except for US
      location_sort_order = ifelse(.data$location == "US", 0, 1)
    ) |>
    dplyr::arrange(.data$location_sort_order, .data$location) |>
    dplyr::left_join(
      population_data,
      by = c("location" = "location_name")
    ) |>
    dplyr::mutate(
      population = as.numeric(.data$population),
      quantile_0.025_per100k = .data$quantile_0.025 / .data$population * 100000,
      quantile_0.5_per100k = .data$quantile_0.5 / .data$population * 100000,
      quantile_0.975_per100k = .data$quantile_0.975 / .data$population * 100000,
      quantile_0.025_count = .data$quantile_0.025,
      quantile_0.5_count = .data$quantile_0.5,
      quantile_0.975_count = .data$quantile_0.975,
      quantile_0.025_per100k_rounded = round(.data$quantile_0.025_per100k, 2),
      quantile_0.5_per100k_rounded = round(.data$quantile_0.5_per100k, 2),
      quantile_0.975_per100k_rounded = round(.data$quantile_0.975_per100k, 2),
      quantile_0.025_count_rounded = round(.data$quantile_0.025_count),
      quantile_0.5_count_rounded = round(.data$quantile_0.5_count),
      quantile_0.975_count_rounded = round(.data$quantile_0.975_count),
      target_end_date_formatted = format(.data$target_end_date, "%B %d, %Y"),
      reference_date_formatted = format(.data$reference_date, "%B %d, %Y"),
      forecast_due_date = as.Date(!!reference_date) - 3,
      forecast_due_date_formatted = format(
        .data$forecast_due_date,
        "%B %d, %Y"
      ),
    ) |>
    dplyr::select(
      location_name = "location",
      "horizon",
      "quantile_0.025_per100k",
      "quantile_0.5_per100k",
      "quantile_0.975_per100k",
      "quantile_0.025_count",
      "quantile_0.5_count",
      "quantile_0.975_count",
      "quantile_0.025_per100k_rounded",
      "quantile_0.5_per100k_rounded",
      "quantile_0.975_per100k_rounded",
      "quantile_0.025_count_rounded",
      "quantile_0.5_count_rounded",
      "quantile_0.975_count_rounded",
      "target",
      "target_end_date",
      "reference_date",
      "forecast_due_date",
      "target_end_date_formatted",
      "forecast_due_date_formatted",
      "reference_date_formatted",
      "model",
    )

  output_folder_path <- fs::path(
    hub_reports_path,
    "weekly-summaries",
    reference_date
  )
  output_filename <- glue::glue("{reference_date}_{file_prefix}_map_data")
  output_filepath <- fs::path(
    output_folder_path,
    output_filename,
    ext = output_format
  )

  fs::dir_create(output_folder_path)
  cli::cli_inform("Directory is ready: {output_folder_path}")

  if (!fs::file_exists(output_filepath)) {
    forecasttools::write_tabular(map_data, output_filepath)
    cli::cli_inform("File saved as: {output_filepath}")
  } else {
    cli::cli_abort("File already exists: {output_filepath}")
  }
}
