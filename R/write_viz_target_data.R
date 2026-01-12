#' Write target data for visualization to disk.
#'
#' This function reads target data from the hub's
#' time-series file using hubData, filters to the latest
#' vintage and specified target, and writes it to the
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
#' @param target Character, target name to filter
#' (e.g., "wk inc covid hosp"). If NULL (default), all
#' targets are included.
#' @param included_locations Character vector of location
#' codes to include in the output. Default
#' hubhelpr::included_locations.
#'
#' @return Invisibly returns file path where data was
#' written. File is named
#' {reference_date}_{disease}_target_data.csv.
#'
#' @export
write_viz_target_data <- function(
  reference_date,
  base_hub_path,
  hub_reports_path,
  disease,
  target = NULL,
  included_locations = hubhelpr::included_locations
) {
  reference_date <- lubridate::as_date(reference_date)

  # connect to hub's target timeseries and get latest vintage

  target_timeseries <- hubData::connect_target_timeseries(base_hub_path)
  target_data <- target_timeseries |>
    forecasttools::hub_target_data_as_of(as_of = "latest") |>
    dplyr::filter(
      .data$location %in% !!included_locations
    ) |>
    dplyr::collect() |>
    dplyr::filter(
      forecasttools::nullable_comparison(.data$target, "==", !!target)
    )

  # add location name and format columns

  target_data <- target_data |>
    dplyr::mutate(
      location_name = forecasttools::us_location_recode(
        .data$location,
        "code",
        "name"
      ),
      # rename "United States" to "US"
      location_name = dplyr::case_match(
        .data$location_name,
        "United States" ~ "US",
        .default = .data$location_name
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
    reference_date
  )
  output_filename <- glue::glue("{reference_date}_{disease}_target_data")
  output_filepath <- fs::path(
    output_folder_path,
    output_filename,
    ext = "csv"
  )

  fs::dir_create(output_folder_path)

  if (!fs::file_exists(output_filepath)) {
    forecasttools::write_tabular(target_data, output_filepath)
    cli::cli_inform("File saved as: {output_filepath}")
  } else {
    cli::cli_abort("File already exists: {output_filepath}")
  }

  invisible(output_filepath)
}
