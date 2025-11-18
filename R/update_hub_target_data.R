nhsn_col_names <- list(
  covid = "totalconfc19newadm",
  rsv = "totalconfrsvnewadm"
)
nssp_col_names <- list(
  covid = "percent_visits_covid",
  rsv = "percent_visits_rsv"
)

#' Get and process target data for a given Hub.
#'
#' This function pulls, formats and save NHSN and NSSP
#' data for use in the Hub.
#'
#' @param base_hub_path Path to the base hub directory.
#' @param disease Disease name ("covid" or "rsv").
#' @param as_of As-of date of the data pull. Default is
#' the system date as determined by [lubridate::today()].
#' @param included_locations Vector of location codes to
#' include in the output.
#' Default value `hubhelpr::included_locations`.
#' @param nhsn_first_weekending_date First week-ending
#' date to include for the NHSN dataset. Default value
#' is "2024-11-09".
#' @param legacy_file Logical. Whether to write legacy
#' CSV output (default: FALSE).
#' @param nssp_update_local Logical. Whether to update NSSP
#' data from local file `auxiliary-data/latest.csv`
#' (default: FALSE).
#'
#' @return Writes `time-series.parquet` and optionally
#' legacy CSV target data files to the target-data
#' directory in the hub.
#' @export
update_hub_target_data <- function(
  base_hub_path,
  disease,
  as_of = lubridate::today(),
  nhsn_first_weekending_date = lubridate::as_date("2024-11-09"),
  included_locations = hubhelpr::included_locations,
  legacy_file = FALSE,
  nssp_update_local = FALSE
) {
  if (!disease %in% c("covid", "rsv")) {
    stop("'disease' must be either 'covid' or 'rsv'")
  }
  output_dirpath <- fs::path(base_hub_path, "target-data")
  fs::dir_create(output_dirpath)

  nhsn_col_name <- nhsn_col_names[[disease]]
  nssp_col_name <- nssp_col_names[[disease]]
  hubverse_ts_req_cols <- c(
    "date",
    "observation",
    "location",
    "as_of",
    "target"
  )
  nhsn_data <- forecasttools::pull_data_cdc_gov_dataset(
    dataset = "nhsn_hrd_prelim",
    columns = nhsn_col_name,
    start_date = nhsn_first_weekending_date
  ) |>
    dplyr::mutate(
      date = lubridate::as_date(.data$weekendingdate),
      observation = as.numeric(.data[[nhsn_col_name]]),
      jurisdiction = stringr::str_replace(.data$jurisdiction, "USA", "US"),
      location = forecasttools::us_location_recode(
        .data$jurisdiction,
        "abbr",
        "code"
      ),
      as_of = !!as_of,
      target = glue::glue("wk inc {disease} hosp")
    ) |>
    dplyr::filter(.data$location %in% !!included_locations)

  hubverse_format_nhsn_data <- nhsn_data |>
    dplyr::select(tidyselect::all_of(hubverse_ts_req_cols))

  if (legacy_file) {
    legacy_file_name <- glue::glue(
      "{disease}-hospital-admissions.csv"
    )
    nhsn_data |>
      dplyr::rename(
        value = "observation",
        state = "jurisdiction"
      ) |>
      dplyr::arrange(state) |>
      dplyr::select("state", "date", "value", "location") |>
      forecasttools::write_tabular(
        fs::path(output_dirpath, legacy_file_name)
      )
  }

  if (nssp_update_local) {
    raw_nssp_data <- forecasttools::read_tabular(
      fs::path(
        base_hub_path,
        "auxiliary-data",
        "nssp-raw-data",
        "latest",
        ext = "csv"
      )
    ) |>
      dplyr::filter(county == "All") |>
      dplyr::select(
        week_end,
        geography,
        dplyr::all_of(nssp_col_name)
      )
  } else {
    raw_nssp_data <- forecasttools::pull_data_cdc_gov_dataset(
      dataset = "nssp_prop_ed_visits",
      columns = c(nssp_col_name, "geography"),
      locations = "All"
    )
  }

  hubverse_format_nssp_data <- raw_nssp_data |>
    dplyr::mutate(
      date = lubridate::as_date(.data$week_end),
      observation = as.numeric(.data[[nssp_col_name]]) / 100,
      location = forecasttools::us_location_recode(
        .data$geography,
        "name",
        "code"
      ),
      as_of = !!as_of,
      target = glue::glue("wk inc {disease} prop ed visits")
    ) |>
    dplyr::filter(.data$location %in% !!included_locations) |>
    dplyr::select(dplyr::all_of(hubverse_ts_req_cols)) |>
    dplyr::arrange(date, location)

  output_file <- fs::path(output_dirpath, "time-series", ext = "parquet")
  if (fs::file_exists(output_file)) {
    existing_data <- forecasttools::read_tabular_file(output_file)
  } else {
    existing_data <- NULL
  }
  dplyr::bind_rows(
    existing_data,
    hubverse_format_nhsn_data,
    hubverse_format_nssp_data
  ) |>
    forecasttools::write_tabular_file(output_file)
}
