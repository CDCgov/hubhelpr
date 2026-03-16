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
#' @param start_date Date, earliest date to include in data.
#' Default: NULL (no filtering). Used only when
#' use_hub_data = FALSE.
#' @param end_date Date, latest date to include in data.
#' Default: NULL (no filtering). Used only when
#' use_hub_data = FALSE.
#' @param excluded_locations Character vector or named list
#' specifying US state abbreviations to exclude. If a
#' character vector, locations are excluded across all
#' targets. If a named list, names should be target names
#' (or "all" for global exclusions) mapping to character
#' vectors of abbreviations. Converted to hub codes
#' internally. Default:
#' [hubhelpr::default_excluded_locations].
#' @param output_format Character, output file format. One
#' of "csv", "tsv", or "parquet". Default: "csv".
#' @param overwrite_existing logical. If TRUE, overwrite
#' existing files. Default: FALSE.
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
  start_date = NULL,
  end_date = NULL,
  excluded_locations = hubhelpr::default_excluded_locations,
  output_format = "csv",
  overwrite_existing = FALSE
) {
  if (use_hub_data) {
    target_data <- hubData::connect_target_timeseries(base_hub_path) |>
      forecasttools::hub_target_data_as_of(as_of = as_of) |>
      dplyr::collect()
  } else {
    nhsn_data <- get_hubverse_format_nhsn_data(
      disease,
      start_date = start_date,
      end_date = end_date
    ) |>
      # remove data for reporting dates May 1, 2024 – October 31,
      # 2024 due to the absence of a reporting mandate. Reporting
      # rates during this period were much lower,
      # and data not comparable to other time periods.
      dplyr::filter(
        !(.data$date >= lubridate::as_date("2024-05-01") &
          .data$date <= lubridate::as_date("2024-10-31"))
      )
    nssp_data <- get_hubverse_format_nssp_data(
      disease,
      base_hub_path,
      start_date = start_date,
      end_date = end_date,
      nssp_update_local = TRUE
    )
    target_data <- dplyr::bind_rows(nhsn_data, nssp_data)
  }

  supported_targets <- get_hub_supported_targets(base_hub_path)
  target_data <- apply_location_exclusions(
    target_data,
    excluded_locations,
    supported_targets
  )

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
        janitor::round_half_up(.data$observation, 4),
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

  if (fs::file_exists(output_filepath) && !overwrite_existing) {
    cli::cli_abort(
      c(
        "File already exists: {output_filepath}.",
        "i" = "Use {.arg overwrite_existing = TRUE} to overwrite."
      )
    )
  }

  forecasttools::write_tabular(target_data, output_filepath)
  cli::cli_inform("File saved as: {output_filepath}.")

  invisible(output_filepath)
}
