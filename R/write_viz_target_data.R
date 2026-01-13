#' Write target data for visualization to disk.
#'
#' This function reads target data from the hub's
#' time-series file using hubData, filters to the latest
#' vintage and specified targets, and writes it to the
#' weekly-summaries directory for use by the visualization
#' webpage. Output includes columns: week_ending_date,
#' location, location_name, target, and value.
#'
#' @param reference_date Character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param base_hub_path Character, path to the forecast hub
#' directory.
#' @param hub_reports_path Character, path to forecast hub
#' reports directory.
#' @param disease Character, disease name ("covid" or "rsv").
#' @param targets Character vector of target names to filter
#' (e.g., "wk inc covid hosp"). If NULL (default), all
#' targets are included.
#' @param included_locations Character vector of location
#' codes to include in the output. Default
#' hubhelpr::included_locations.
#' @param output_format Character, output file format. One
#' of "csv", "tsv", or "parquet". Default: "csv".
#'
#' @return Invisibly returns file path where data was
#' written.
#'
#' @export
write_viz_target_data <- function(
  reference_date,
  base_hub_path,
  hub_reports_path,
  disease,
  targets = NULL,
  included_locations = hubhelpr::included_locations,
  output_format = "csv"
) {
  target_data <- hubData::connect_target_timeseries(base_hub_path) |>
    forecasttools::hub_target_data_as_of(as_of = "latest") |>
    dplyr::filter(.data$location %in% !!included_locations) |>
    dplyr::collect() |>
    dplyr::filter(
      forecasttools::nullable_comparison(.data$target, "%in%", !!targets)
    ) |>
    dplyr::mutate(
      location_name = forecasttools::us_location_recode(
        .data$location,
        "code",
        "name"
      ),
      observation = dplyr::if_else(
        grepl("ed visits", .data$target),
        round(.data$observation, 4),
        .data$observation
      )
    ) |>
    dplyr::select(
      week_ending_date = "date",
      "location",
      "location_name",
      "target",
      value = "observation"
    )

  # create output directory and file path
  output_folder_path <- fs::path(
    hub_reports_path,
    "weekly-summaries",
    get_hub_repo_name(disease),
    reference_date
  )
  output_filename <- glue::glue("{reference_date}_{disease}_target_data")
  output_filepath <- fs::path(
    output_folder_path,
    output_filename,
    ext = output_format
  )

  fs::dir_create(output_folder_path)

  if (!fs::file_exists(output_filepath)) {
    forecasttools::write_tabular(target_data, output_filepath)
    cli::cli_inform("File saved as: {output_filepath}.")
  } else {
    cli::cli_abort("File already exists: {output_filepath}.")
  }

  invisible(output_filepath)
}
