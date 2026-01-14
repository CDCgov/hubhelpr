#' Write forecast summary to disk.
#'
#' This function calls `summarize_ref_date_forecasts()` and writes the
#' resulting tibble to disk in the specified format.
#'
#' @param reference_date character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param base_hub_path character, path to the forecast hub
#' directory.
#' @param hub_reports_path character, path to forecast hub
#' reports directory.
#' @param disease character, disease name ("covid" or
#' "rsv").
#' @param file_suffix character, suffix to append to
#' filename (e.g., "map_data", "forecasts_data").
#' @param horizons_to_include integer vector, horizons to
#' include in the output. Default: c(0, 1, 2).
#' @param excluded_locations character vector of location
#' codes to exclude from the output. Default: character(0).
#' @param output_format character, output file format. One
#' of "csv", "tsv", or "parquet". Default: "csv".
#' @param targets character vector, target name(s) to
#' filter forecasts. If NULL (default), does not filter by
#' target.
#' @param model_ids character vector of model IDs to
#' include. If NULL (default), includes all models.
#' @param population_data data frame with columns
#' "location" and "population".
#' @param column_selection Columns to include in the output
#' table. Accepts tidyselect expressions. Default:
#' [tidyselect::everything()].
#'
#' @return invisibly returns the file path where data was
#' written
#'
#' @export
write_ref_date_summary <- function(
  reference_date,
  base_hub_path,
  hub_reports_path,
  disease,
  file_suffix,
  horizons_to_include = c(0, 1, 2),
  excluded_locations = character(0),
  output_format = "csv",
  targets = NULL,
  model_ids = NULL,
  population_data,
  column_selection = tidyselect::everything()
) {
  reference_date <- lubridate::as_date(reference_date)

  summary_data <- summarize_ref_date_forecasts(
    reference_date = reference_date,
    base_hub_path = base_hub_path,
    disease = disease,
    population_data = population_data,
    horizons_to_include = horizons_to_include,
    excluded_locations = excluded_locations,
    targets = targets,
    model_ids = model_ids
  )

  summary_data <- summary_data |>
    dplyr::select({{ column_selection }})

  output_folder_path <- fs::path(
    hub_reports_path,
    "weekly-summaries",
    get_hub_repo_name(disease),
    reference_date
  )
  output_filename <- glue::glue("{reference_date}_{disease}_{file_suffix}")
  output_filepath <- fs::path(
    output_folder_path,
    output_filename,
    ext = output_format
  )

  fs::dir_create(output_folder_path)

  if (!fs::file_exists(output_filepath)) {
    forecasttools::write_tabular(summary_data, output_filepath)
    cli::cli_inform("File saved as: {output_filepath}")
  } else {
    cli::cli_abort("File already exists: {output_filepath}")
  }

  invisible(output_filepath)
}


#' Write ensemble forecast summary to disk.
#'
#' This function generates and writes ensemble-only forecast data.
#'
#' @param reference_date character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param base_hub_path character, path to the forecast hub
#' directory.
#' @param hub_reports_path character, path to forecast hub
#' reports directory.
#' @param disease character, disease name ("covid" or "rsv").
#' @param horizons_to_include integer vector, horizons to
#' include in the output. Default: c(0, 1, 2).
#' @param population_data data frame with columns
#' "location" and "population". Default: population_data.
#' @param excluded_locations character vector of location
#' codes to exclude from the output. Default: character(0).
#' @param output_format character, output file format. One
#' of "csv", "tsv", or "parquet". Default: "csv".
#' @param targets character vector, target name(s) to
#' filter forecasts. If NULL (default), does not filter by
#' target.
#'
#' @return invisibly returns the file path where data was
#' written
#'
#' @export
write_ref_date_summary_ens <- function(
  reference_date,
  base_hub_path,
  hub_reports_path,
  disease,
  horizons_to_include = c(0, 1, 2),
  population_data = hubhelpr::population_data,
  excluded_locations = character(0),
  output_format = "csv",
  targets = NULL
) {
  hub_name <- get_hub_name(disease)
  ensemble_model_name <- glue::glue("{hub_name}-ensemble")

  ensemble_columns <- c(
    "location_name",
    "horizon",
    "quantile_0.025_per100k",
    "quantile_0.5_per100k",
    "quantile_0.975_per100k",
    "quantile_0.025",
    "quantile_0.5",
    "quantile_0.975",
    "quantile_0.025_per100k_rounded",
    "quantile_0.5_per100k_rounded",
    "quantile_0.975_per100k_rounded",
    "quantile_0.025_rounded",
    "quantile_0.5_rounded",
    "quantile_0.975_rounded",
    "target",
    "target_end_date",
    "reference_date",
    "forecast_due_date",
    "target_end_date_formatted",
    "forecast_due_date_formatted",
    "reference_date_formatted",
    model = "model_id"
  )

  write_ref_date_summary(
    reference_date = reference_date,
    base_hub_path = base_hub_path,
    hub_reports_path = hub_reports_path,
    disease = disease,
    file_suffix = "map_data",
    horizons_to_include = horizons_to_include,
    excluded_locations = excluded_locations,
    output_format = output_format,
    targets = targets,
    model_ids = ensemble_model_name,
    population_data = population_data,
    column_selection = ensemble_columns
  )
}


#' Write all-models forecast summary to disk.
#'
#' This function generates and writes forecast data for all models.
#'
#' @param reference_date character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param base_hub_path character, path to the forecast hub
#' directory.
#' @param hub_reports_path character, path to forecast hub
#' reports directory.
#' @param disease character, disease name ("covid" or "rsv").
#' @param horizons_to_include integer vector, horizons to
#' include in the output. Default: c(0, 1, 2).
#' @param population_data data frame with columns
#' "location" and "population". Default: [population_data].
#' @param excluded_locations character vector of location
#' codes to exclude from the output. Default: character(0).
#' @param output_format character, output file format. One
#' of "csv", "tsv", or "parquet". Default: "csv".
#' @param targets character vector, target name(s) to
#' filter forecasts. If NULL (default), does not filter by
#' target.
#'
#' @return invisibly returns the file path where data was
#' written
#'
#' @export
write_ref_date_summary_all <- function(
  reference_date,
  base_hub_path,
  hub_reports_path,
  disease,
  horizons_to_include = c(0, 1, 2),
  population_data = population_data,
  excluded_locations = character(0),
  output_format = "csv",
  targets = NULL
) {
  all_models_columns <- c(
    "location_name",
    "abbreviation",
    "horizon",
    "target",
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

  write_ref_date_summary(
    reference_date = reference_date,
    base_hub_path = base_hub_path,
    hub_reports_path = hub_reports_path,
    disease = disease,
    file_suffix = "forecasts_data",
    horizons_to_include = horizons_to_include,
    excluded_locations = excluded_locations,
    output_format = output_format,
    targets = targets,
    model_ids = NULL,
    population_data = population_data,
    column_selection = all_models_columns
  )
}
