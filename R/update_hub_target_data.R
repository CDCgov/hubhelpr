hubverse_ts_req_cols <- c(
  "date",
  "observation",
  "location",
  "as_of",
  "target"
)

#' Get and format NHSN data for a given disease.
#'
#' This function pulls the NHSN hospital admissions data,
#' formats and returns it in the hubverse format.
#'
#' @param disease Disease name ("covid" or "rsv").
#' @param as_of As-of date of the data pull. Default is
#' the system date as determined by [lubridate::today()].
#' @param included_locations Vector of location codes to
#' include in the output.
#' Default value `hubhelpr::included_locations`.
#' @param start_date First week-ending
#' date to include for the NHSN dataset. Default value
#' is NULL (no filtering).
#' @param end_date Last week-ending
#' date to include for the NHSN dataset. Default value
#' is NULL (no filtering).
#'
#' @return Data frame with formatted NHSN data.
#' @export
get_hubverse_format_nhsn_data <- function(
  disease,
  as_of = lubridate::today(),
  included_locations = hubhelpr::included_locations,
  start_date = NULL,
  end_date = NULL
) {
  checkmate::assert_scalar(disease)
  checkmate::assert_choice(disease, choices = c("covid", "rsv", "flu"))

  nhsn_col_name <- get_nhsn_col_name(disease)

  hubverse_format_nhsn_data <- forecasttools::pull_data_cdc_gov_dataset(
    dataset = "nhsn_hrd_prelim",
    columns = nhsn_col_name,
    start_date = start_date,
    end_date = end_date
  ) |>
    dplyr::mutate(
      date = lubridate::as_date(.data$weekendingdate),
      observation = as.numeric(.data[[nhsn_col_name]]),
      location = forecasttools::us_location_recode(
        .data$jurisdiction,
        "hrd",
        "code"
      ),
      as_of = !!as_of,
      target = glue::glue("wk inc {disease} hosp")
    ) |>
    dplyr::filter(.data$location %in% !!included_locations) |>
    dplyr::select(tidyselect::all_of(hubverse_ts_req_cols))

  return(hubverse_format_nhsn_data)
}

#' Get and format NSSP data for a given disease.
#'
#' This function pulls, formats and returns NSSP emergency
#' department visit data in hubverse format.
#'
#' @param disease Disease name ("covid" or "rsv").
#' @param base_hub_path Path to the base hub directory.
#' @param as_of As-of date of the data pull. Default is
#' the system date as determined by [lubridate::today()].
#' @param included_locations Vector of location codes to
#' include in the output.
#' Default value `hubhelpr::included_locations`.
#' @param nssp_update_local Logical. Whether to update NSSP
#' data from local file `auxiliary-data/latest.parquet`
#' (default: FALSE).
#' @param start_date First week-ending
#' date to include for the NSSP dataset. Default value
#' is NULL (no filtering).
#' @param end_date Last week-ending
#' date to include for the NSSP dataset. Default value
#' is NULL (no filtering).
#'
#' @return Data frame with formatted NSSP data.
#' @export
get_hubverse_format_nssp_data <- function(
  disease,
  base_hub_path,
  as_of = lubridate::today(),
  included_locations = hubhelpr::included_locations,
  nssp_update_local = FALSE,
  start_date = NULL,
  end_date = NULL
) {
  checkmate::assert_scalar(disease)
  checkmate::assert_choice(disease, choices = c("covid", "rsv", "flu"))

  nssp_col_name <- get_nssp_col_name(disease)

  if (nssp_update_local) {
    nssp_file_path <- fs::path(
      base_hub_path,
      "auxiliary-data",
      "nssp-raw-data",
      "latest",
      ext = "parquet"
    )

    raw_nssp_data <- forecasttools::read_tabular(nssp_file_path) |>
      dplyr::filter(
        .data$county == "All",
        forecasttools::nullable_comparison(.data$week_end, ">=", !!start_date),
        forecasttools::nullable_comparison(.data$week_end, "<=", !!end_date)
      ) |>
      dplyr::select(
        "week_end",
        "geography",
        tidyselect::all_of(nssp_col_name)
      )
  } else {
    raw_nssp_data <- forecasttools::pull_data_cdc_gov_dataset(
      dataset = "nssp_prop_ed_visits",
      columns = c(nssp_col_name, "geography"),
      locations = "All",
      start_date = start_date,
      end_date = end_date
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
    dplyr::select(tidyselect::all_of(hubverse_ts_req_cols)) |>
    dplyr::arrange(.data$date, .data$location)

  return(hubverse_format_nssp_data)
}


#' Update hub target data.
#'
#' This function pulls, formats and saves NHSN and NSSP
#' data for use in the Hub. It combines both data sources
#' with any existing data and writes to the target-data directory.
#'
#' @param base_hub_path Path to the base hub directory.
#' @param disease Disease name ("covid" or "rsv").
#' @param as_of As-of date of the data pull. Default is
#' the system date as determined by [lubridate::today()].
#' @param start_date First week-ending
#' date to include for the NHSN dataset. Default value
#' is "2024-11-09".
#' @param included_locations Vector of location codes to
#' include in the output.
#' Default value `hubhelpr::included_locations`.
#' @param legacy_file Logical. Whether to write legacy
#' CSV output (default: FALSE).
#' @param nssp_update_local Logical. Whether to update NSSP
#' data from local hub file `auxiliary-data/latest.parquet`
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
  start_date = lubridate::as_date("2024-11-09"),
  included_locations = hubhelpr::included_locations,
  legacy_file = FALSE,
  nssp_update_local = FALSE
) {
  checkmate::assert_scalar(disease)
  checkmate::assert_choice(disease, choices = c("covid", "rsv"))

  nhsn_data <- get_hubverse_format_nhsn_data(
    disease,
    as_of = as_of,
    included_locations = included_locations,
    start_date = start_date
  )

  assert_data_up_to_date(
    nhsn_data,
    location_col_name = "location",
    date_col_name = "date",
    expected_max_time_value = forecasttools::floor_mmwr_epiweek(as_of) -
      lubridate::days(1)
  )

  nssp_data <- get_hubverse_format_nssp_data(
    disease,
    base_hub_path,
    as_of = as_of,
    included_locations = included_locations,
    nssp_update_local = nssp_update_local
  )

  assert_data_up_to_date(
    nssp_data,
    location_col_name = "location",
    date_col_name = "date",
    expected_max_time_value = forecasttools::floor_mmwr_epiweek(as_of) -
      lubridate::days(1)
  )

  output_dirpath <- fs::path(base_hub_path, "target-data")
  fs::dir_create(output_dirpath)

  if (legacy_file) {
    legacy_file_name <- glue::glue(
      "{disease}-hospital-admissions.csv"
    )
    nhsn_data |>
      dplyr::mutate(
        state = forecasttools::us_location_recode(
          .data$location,
          "code",
          "abbr"
        )
      ) |>
      dplyr::rename(
        value = "observation"
      ) |>
      dplyr::arrange(.data$state) |>
      dplyr::select("state", "date", "value", "location") |>
      forecasttools::write_tabular(
        fs::path(output_dirpath, legacy_file_name)
      )
  }

  output_file <- fs::path(output_dirpath, "time-series", ext = "parquet")

  if (fs::file_exists(output_file)) {
    existing_data <- forecasttools::read_tabular_file(output_file)
  } else {
    existing_data <- NULL
  }
  dplyr::bind_rows(
    existing_data,
    nhsn_data,
    nssp_data
  ) |>
    forecasttools::write_tabular_file(output_file)

  return(invisible())
}
