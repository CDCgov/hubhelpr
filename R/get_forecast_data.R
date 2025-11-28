#' Generate forecast data file containing all forecast hub
#' model submissions.
#'
#' This function fetches all forecast submissions from a
#' forecast hub based on the reference date. The forecast
#' data is then pivoted to create a wide format with
#' quantile levels as columns.
#'
#' The resulting file contains the following columns:
#' - `location_name`: full state name (including "US" for
#'    the US state)
#' - `abbreviation`: state abbreviation
#' - `horizon`: forecast horizon
#' - `forecast_date`: date the forecast was generated
#' - `target_end_date`: target date for the forecast
#' - `model`: model name
#' - `quantile_*`: forecast values for various quantiles
#'    (e.g., 0.025, 0.5, 0.975)
#' - `forecast_teams`: name of the team that generated the
#'    model
#' - `forecast_fullnames`: full model name
#'
#' @param reference_date character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param base_hub_path character, path to the forecast
#' hub directory.
#' @param hub_reports_path character, path to forecast hub
#' reports directory.
#' @param disease character, disease name ("covid" or
#' "rsv"). Used to derive target name and file prefix.
#' @param horizons_to_include integer vector, horizons to
#' include in the output. Default: c(0, 1, 2).
#' @param excluded_locations character vector of location
#' codes to exclude from the output. Default: character(0).
#' @param output_format character, output file format. One
#' of "csv", "tsv", or "parquet". Default: "csv".
#' @param targets character vector, target name(s) to filter
#' forecasts. If NULL (default), does not filter by target.
#' Can be a single target like "wk inc covid hosp" or
#' multiple targets like c("wk inc covid hosp", "wk inc
#' covid prop ed visits").
#'
#' @export
get_forecast_data <- function(
  reference_date,
  base_hub_path,
  hub_reports_path,
  disease,
  horizons_to_include = c(0, 1, 2),
  excluded_locations = character(0),
  output_format = "csv",
  targets = NULL
) {
  checkmate::assert_choice(disease, choices = c("covid", "rsv"))
  checkmate::assert_subset(horizons_to_include, choices = c(-1, 0, 1, 2, 3))
  checkmate::assert_character(excluded_locations)
  checkmate::assert_choice(output_format, choices = c("csv", "tsv", "parquet"))
  checkmate::assert_character(targets, null.ok = TRUE)

  reference_date <- lubridate::as_date(reference_date)

  model_metadata <- hubData::load_model_metadata(
    base_hub_path,
    model_ids = NULL
  )

  hub_content <- hubData::connect_hub(base_hub_path)

  current_forecasts <- hub_content |>
    dplyr::filter(
      .data$reference_date == !!reference_date,
      !(.data$location %in% !!excluded_locations),
      .data$horizon %in% !!horizons_to_include,
      forecasttools::nullable_comparison(
        .data$target,
        "%in%",
        !!targets
      )
    ) |>
    hubData::collect_hub()

  all_forecasts_data <- forecasttools::pivot_hubverse_quantiles_wider(
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
      ),
      dplyr::across(
        tidyselect::starts_with("quantile_"),
        round,
        .names = "{.col}_rounded"
      ),
      forecast_due_date = as.Date(!!reference_date) - 3,
      location_sort_order = ifelse(.data$location_name == "United States", 0, 1)
    ) |>
    dplyr::mutate(
      location_name = dplyr::case_match(
        .data$location_name,
        "United States" ~ "US",
        .default = .data$location_name
      )
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
    dplyr::select(
      "location_name",
      "abbreviation",
      "horizon",
      forecast_date = "reference_date",
      "target_end_date",
      model = "model_id",
      "quantile_0.025",
      "quantile_0.25",
      "quantile_0.5",
      "quantile_0.75",
      "quantile_0.975",
      "quantile_0.025_rounded",
      "quantile_0.25_rounded",
      "quantile_0.5_rounded",
      "quantile_0.75_rounded",
      "quantile_0.975_rounded",
      forecast_team = "team_name",
      "forecast_due_date",
      model_full_name = "model_name"
    )

  output_folder_path <- fs::path(
    hub_reports_path,
    "weekly-summaries",
    reference_date
  )
  output_filename <- glue::glue("{reference_date}_{disease}_forecasts_data")
  output_filepath <- fs::path(
    output_folder_path,
    output_filename,
    ext = output_format
  )

  fs::dir_create(output_folder_path)

  if (!fs::file_exists(output_filepath)) {
    forecasttools::write_tabular(all_forecasts_data, output_filepath)
    cli::cli_inform("File saved as: {output_filepath}")
  } else {
    cli::cli_abort("File already exists: {output_filepath}")
  }
}
