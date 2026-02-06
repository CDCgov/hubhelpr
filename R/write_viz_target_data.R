#' Write target data for visualization to disk.
#'
#' This function reads target data either from the hub's
#' time-series file using hubData or by pulling data
#' from NHSN and NSSP APIs, then writes to the
#' weekly-summaries directory for use by the visualization
#' webpage. Output includes columns: week_ending_date,
#' location, location_name, target, target_data_type, and value.
#'
#' @param reference_date Character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param base_hub_path Character, path to the forecast hub
#' directory.
#' @param hub_reports_path Character, path to forecast hub
#' reports directory.
#' @param disease Character, disease name ("covid" or "rsv").
#' @param use_hub_data Logical, whether to read data from
#' the hub's time-series file (TRUE) or pull fresh data
#' from sources (FALSE). Default: FALSE.
#' @param as_of As of date to filter to, as an object coercible
#' by as.Date(), or "latest" to filter to the most recent
#' available vintage. Default "latest". Used only when
#' use_hub_data = TRUE.
#' @param pull_nhsn Logical, whether to pull NHSN hospital
#' admissions data. Default: TRUE. Used only when
#' use_hub_data = FALSE.
#' @param pull_nssp Logical, whether to pull NSSP emergency
#' department visit data. Default: TRUE. Used only when
#' use_hub_data = FALSE.
#' @param start_date Date, earliest date to include in data.
#' Default: NULL (no filtering). Used only when
#' use_hub_data = FALSE.
#' @param end_date Date, latest date to include in data.
#' Default: NULL (no filtering). Used only when
#' use_hub_data = FALSE.
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
  use_hub_data = FALSE,
  as_of = "latest",
  pull_nhsn = TRUE,
  pull_nssp = TRUE,
  start_date = NULL,
  end_date = NULL,
  included_locations = hubhelpr::included_locations,
  output_format = "csv"
) {
  if (use_hub_data) {
    target_data <- hubData::connect_target_timeseries(base_hub_path) |>
      forecasttools::hub_target_data_as_of(as_of = as_of) |>
      dplyr::filter(.data$location %in% !!included_locations) |>
      dplyr::collect()
  } else {
    if (!pull_nhsn && !pull_nssp) {
      cli::cli_abort(
        "When 'use_hub_data' is FALSE, at least
        one of 'pull_nhsn' or 'pull_nssp' must be TRUE"
      )
    }

    nhsn_data <- if (pull_nhsn) {
      get_hubverse_format_nhsn_data(
        disease,
        start_date = start_date,
        end_date = end_date
      )
    } else {
      NULL
    }
    nssp_data <- if (pull_nssp) {
      get_hubverse_format_nssp_data(
        disease,
        base_hub_path,
        start_date = start_date,
        end_date = end_date,
        nssp_update_local = TRUE
      )
    } else {
      NULL
    }
    target_data <- dplyr::bind_rows(nhsn_data, nssp_data)
  }

  target_data <- target_data |>
    dplyr::mutate(
      location_name = forecasttools::us_location_recode(
        .data$location,
        "code",
        "name"
      ),
      target_data_type = get_target_data_type(.data$target)
    ) |>
    dplyr::mutate(
      observation = dplyr::if_else(
        .data$target_data_type == "prop_ed",
        round(.data$observation, 4),
        .data$observation
      )
    ) |>
    dplyr::select(
      week_ending_date = "date",
      "location",
      "location_name",
      "target",
      "target_data_type",
      value = "observation"
    )

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
