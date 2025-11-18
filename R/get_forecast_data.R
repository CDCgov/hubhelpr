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
#' @param horizons_to_include integer vector, horizons to
#' include in the output. Default: c(0, 1, 2).
#' @param file_prefix character, prefix used in output
#' filename (e.g., "covid", "rsv"). Default: "covid".
#' @param target_name character, target name to filter
#' forecasts e.g., "wk inc covid hosp", "wk inc rsv hosp").
#' Default: "wk inc covid hosp".
#'
#' @export
get_forecast_data <- function(
  reference_date,
  base_hub_path,
  hub_reports_path,
  horizons_to_include = c(0, 1, 2),
  file_prefix = "covid",
  target_name = "wk inc covid hosp"
) {
  # check for invalid horizon entries
  valid_horizons <- c(-1, 0, 1, 2, 3)
  invalid_horizons <- horizons_to_include[
    !sapply(
      horizons_to_include,
      function(x) x %in% valid_horizons
    )
  ]
  if (length(invalid_horizons) > 0) {
    stop(
      "Invalid elements: ",
      glue::glue_collapse(invalid_horizons, sep = ", ")
    )
  }

  # create model metadata path
  model_metadata <- hubData::load_model_metadata(
    base_hub_path,
    model_ids = NULL
  )

  # get forecast hub content
  hub_content <- hubData::connect_hub(base_hub_path)

  # check if the reference date has any
  # exclusions and exclude specified locations (if any)
  exclude_data_path_toml <- fs::path(
    base_hub_path,
    "auxiliary-data",
    "excluded_locations.toml"
  )
  if (fs::file_exists(exclude_data_path_toml)) {
    exclude_data_toml <- RcppTOML::parseTOML(exclude_data_path_toml)
    if (reference_date %in% names(exclude_data_toml)) {
      excluded_locations <- exclude_data_toml[[reference_date]]
      message("Excluding locations for reference date: ", reference_date)
      current_forecasts <- hub_content |>
        dplyr::filter(reference_date == as.Date(!!reference_date)) |>
        dplyr::filter(!(location %in% excluded_locations)) |>
        dplyr::filter(target == !!target_name) |>
        hubData::collect_hub()
    } else {
      message("No exclusions for reference date: ", reference_date)
      current_forecasts <- hub_content |>
        dplyr::filter(reference_date == as.Date(!!reference_date)) |>
        dplyr::filter(target == !!target_name) |>
        hubData::collect_hub()
    }
  } else {
    stop("TOML file not found: ", exclude_data_path_toml)
  }

  # get data for All Forecasts file
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
    # usually filter out horizon 3, -1
    dplyr::filter(horizon %in% !!horizons_to_include) |>
    # convert location codes to full location
    # names and to abbreviations
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
      # round the quantiles to nearest integer
      # for rounded versions
      dplyr::across(
        tidyselect::starts_with("quantile_"),
        round,
        .names = "{.col}_rounded"
      ),
      forecast_due_date = as.Date(!!reference_date) - 3,
      location_sort_order = ifelse(.data$location_name == "United States", 0, 1)
    ) |>
    # long name "United States" to "US"
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
      ), # duplicate model_ids
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

  # output folder and file paths for All Forecasts
  output_folder_path <- fs::path(
    hub_reports_path,
    "weekly-summaries",
    reference_date
  )
  output_filename <- glue::glue(
    "{reference_date}_{file_prefix}_forecasts_data.csv"
  )
  output_filepath <- fs::path(output_folder_path, output_filename)

  # determine if the output folder exists,
  # create it if not
  fs::dir_create(output_folder_path)
  message("Directory is ready: ", output_folder_path)

  # check if the file exists, and if not,
  # save to csv, else throw an error
  if (!fs::file_exists(output_filepath)) {
    readr::write_csv(all_forecasts_data, output_filepath)
    message("File saved as: ", output_filepath)
  } else {
    stop("File already exists: ", output_filepath)
  }
}
