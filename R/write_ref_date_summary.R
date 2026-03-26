#' Write forecast summary to disk.
#'
#' This helper writes a summary data frame to disk in the specified
#' format.
#'
#' @param summary_data Summary of forecast data
#' for a given reference date, usually the output of
#' summarize_ref_date_forecasts() after column selection.
#' Must include: `location_name`, `horizon`,
#' `target`, `target_data_type`, `quantile_*`, `target_end_date`,
#' `forecast_due_date`, and `model`.
#' @param reference_date character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param hub_reports_path character, path to forecast hub
#' reports directory.
#' @param disease character, disease name ("covid" or
#' "rsv").
#' @param file_suffix character, suffix to append to
#' filename (e.g., "map_data", "forecasts_data").
#' @param output_format character, output file format. One
#' of "csv", "tsv", or "parquet". Default: "csv".
#' @param overwrite_existing logical. If TRUE, overwrite
#' existing files. Default: FALSE.
#'
#' @return invisibly returns the file path where data was
#' written
#'
#' @export
write_ref_date_summary <- function(
  summary_data,
  reference_date,
  hub_reports_path,
  disease,
  file_suffix,
  output_format = "csv",
  overwrite_existing = FALSE
) {
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

  if (fs::file_exists(output_filepath) && !overwrite_existing) {
    cli::cli_abort(
      c(
        "File already exists: {output_filepath}.",
        "i" = "Use {.arg overwrite_existing = TRUE} to overwrite."
      )
    )
  }

  forecasttools::write_tabular(summary_data, output_filepath)
  cli::cli_inform("File saved as: {output_filepath}.")

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
#' @param excluded_locations character vector or named list
#' specifying US state abbreviations to exclude. If a
#' character vector, locations are excluded across all
#' targets. If a named list, names should be target names
#' (or "all" for global exclusions) mapping to character
#' vectors of abbreviations. Default: NULL.
#' @param output_format character, output file format. One
#' of "csv", "tsv", or "parquet". Default: "csv".
#' @param targets character vector, target name(s) to
#' filter forecasts. If NULL (default), does not filter by
#' target.
#' @param n_models_for_reporting integer, minimum number of
#' designated model submissions required to include an
#' ensemble forecast in the report. Default: 2.
#' @param overwrite_existing logical. If TRUE, overwrite
#' existing files. Default: FALSE.
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
  excluded_locations = NULL,
  output_format = "csv",
  targets = NULL,
  n_models_for_reporting = 2,
  overwrite_existing = FALSE
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
    "target_data_type",
    "target_end_date",
    "reference_date",
    "forecast_due_date",
    "target_end_date_formatted",
    "forecast_due_date_formatted",
    "reference_date_formatted",
    model = "model_id"
  )

  summary_data <- summarize_ref_date_forecasts(
    reference_date = reference_date,
    base_hub_path = base_hub_path,
    disease = disease,
    population_data = population_data,
    horizons_to_include = horizons_to_include,
    excluded_locations = excluded_locations,
    targets = targets,
    model_ids = ensemble_model_name
  )

  reportable_forecasts <- count_designated_models(
    reference_dates = reference_date,
    base_hub_path = base_hub_path,
    targets = targets,
    horizons = horizons_to_include
  ) |>
    dplyr::filter(.data$n_models >= !!n_models_for_reporting) |>
    dplyr::select(-"n_models")

  summary_data |>
    dplyr::inner_join(
      reportable_forecasts,
      by = c("reference_date", "target", "location", "horizon")
    ) |>
    dplyr::select({{ ensemble_columns }}) |>
    write_ref_date_summary(
      reference_date = reference_date,
      hub_reports_path = hub_reports_path,
      disease = disease,
      file_suffix = "map_data",
      output_format = output_format,
      overwrite_existing = overwrite_existing
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
#' @param excluded_locations character vector or named list
#' specifying US state abbreviations to exclude. If a
#' character vector, locations are excluded across all
#' targets. If a named list, names should be target names
#' (or "all" for global exclusions) mapping to character
#' vectors of abbreviations. Default: NULL.
#' @param output_format character, output file format. One
#' of "csv", "tsv", or "parquet". Default: "csv".
#' @param targets character vector, target name(s) to
#' filter forecasts. If NULL (default), does not filter by
#' target.
#' @param overwrite_existing logical. If TRUE, overwrite
#' existing files. Default: FALSE.
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
  population_data = hubhelpr::population_data,
  excluded_locations = NULL,
  output_format = "csv",
  targets = NULL,
  overwrite_existing = FALSE
) {
  all_models_columns <- c(
    "location_name",
    "abbreviation",
    "horizon",
    "target",
    "target_data_type",
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

  summarize_ref_date_forecasts(
    reference_date = reference_date,
    base_hub_path = base_hub_path,
    disease = disease,
    population_data = population_data,
    horizons_to_include = horizons_to_include,
    excluded_locations = excluded_locations,
    targets = targets
  ) |>
    dplyr::select({{ all_models_columns }}) |>
    write_ref_date_summary(
      reference_date = reference_date,
      hub_reports_path = hub_reports_path,
      disease = disease,
      file_suffix = "forecasts_data",
      output_format = output_format,
      overwrite_existing = overwrite_existing
    )
}
