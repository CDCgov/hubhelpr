nhsn_col_names <- list(
  covid = "totalconfc19newadm",
  rsv = "totalconfrsvnewadm"
)
nssp_col_names <- list(
  covid = "percent_visits_covid",
  rsv = "percent_visits_rsv"
)

##' Get and process target data for the hub
##'
##' This function pulls, formats and save NHSN and NSSP data for use in the hub.
##'
##' @param base_hub_path Path to the base hub directory.
##' @param disease Disease name (e.g., "covid", "rsv").
##' @param excluded_locations Vector of location codes to exclude from the output.
##' @param first_full_weekending_date First week-ending date to include.
##' @param legacy_file Logical. Whether to write legacy CSV output (default: FALSE).
##'
##' @returns Writes target data files to target-data directory in the hub.
##' @export
update_hub_target_data <- function(
  base_hub_path,
  disease,
  first_full_weekending_date = lubridate::as_date("2024-11-09"),
  excluded_locations = c("78", "0", "69", "66", "60"),
  legacy_file = FALSE
) {
  today <- lubridate::today()
  output_dirpath <- fs::path(base_hub_path, "target-data")
  fs::dir_create(output_dirpath)

  nhsn_col_name <- nhsn_col_names[[disease]]
  nssp_col_name <- nssp_col_names[[disease]]

  nhsn_data <- forecasttools::pull_data_cdc_gov_dataset(
    dataset = "nhsn_hrd_prelim",
    columns = nhsn_col_name,
    start_date = first_full_weekending_date
  ) |>
    dplyr::rename(
      observation = nhsn_col_name,
      date = "weekendingdate"
    ) |>
    dplyr::mutate(
      date = as.Date(.data$date),
      observation = as.numeric(.data$observation),
      jurisdiction = stringr::str_replace(.data$jurisdiction, "USA", "US")
    ) |>
    dplyr::mutate(
      location = forecasttools::us_location_recode(
        .data$jurisdiction,
        "abbr",
        "code"
      ),
      as_of = !!today,
      target = glue::glue("wk inc {disease} hosp")
    ) |>
    dplyr::filter(!(location %in% !!excluded_locations))

  hubverse_format_nhsn_data <- nhsn_data |> dplyr::select(-"jurisdiction")

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
      dplyr::select(-c("as_of", "target")) |>
      readr::write_csv(
        fs::path(output_dirpath, legacy_file_name)
      )
  }

  hubverse_format_nssp_data <- forecasttools::pull_data_cdc_gov_dataset(
    dataset = "nssp_prop_ed_visits",
    columns = c(nssp_col_name, "geography"),
    locations = "All"
  ) |>
    dplyr::mutate(
      date = as.Date(.data$week_end),
      observation = as.numeric(.data[[nssp_col_name]]) / 100,
      location = forecasttools::us_location_recode(
        .data$geography,
        "name",
        "code"
      ),
      as_of = !!today,
      target = glue::glue("wk inc {disease} prop ed visits")
    ) |>
    dplyr::select(all_of(c(
      "date",
      "observation",
      "location",
      "as_of",
      "target"
    ))) |>
    dplyr::arrange(date, location)

  output_file <- fs::path(output_dirpath, "time-series", ext = "parquet")
  forecasttools::read_tabular_file(output_file) |>
    dplyr::bind_rows(hubverse_format_nhsn_data, hubverse_format_nssp_data) |>
    dplyr::distinct() |>
    forecasttools::write_tabular_file(output_file)
}
