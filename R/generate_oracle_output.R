#' Create an oracle output table from a Hub
#'
#' @param hub_path Path to the hub root.
#' @param ts_date_col Name of the date column in
#' the `time-series` format target data. Will be converted
#' to `target_end_date` for the oracle output. Default `date`.
#' @return the oracle output table, as a [`tibble`][tibble::tibble()].
#' @export
generate_oracle_output_table <- function(hub_path, ts_date_col = "date") {
  target_ts <- hubData::connect_target_timeseries(hub_path)

  ## need one row for each unique combination of target_end_date
  ## and predictable quantity. Do not need repeated rows for, e.g.
  ## incident admissions on 2025-01-01 as predicted on two different
  ## reference dates (i.e. same predictable quantity and target end
  ## date, but distinct reference dates / horizons)
  required_oracle_values <- get_hub_tasks(hub_path) |>
    dplyr::mutate(
      target_end_date = forecasttools::target_end_dates_from_horizons(
        .data$reference_date,
        .data$horizon,
        "weeks"
      )
    ) |>
    dplyr::select(-"reference_date", -"horizon") |>
    dplyr::distinct()

  target_data <- target_ts |>
    forecasttools::hub_target_data_as_of("latest", .drop = TRUE) |>
    dplyr::collect() |>
    dplyr::rename(target_end_date = !!ts_date_col)

  join_key <- intersect(
    colnames(required_oracle_values),
    colnames(target_data)
  )

  oracle_output <- dplyr::inner_join(
    required_oracle_values,
    target_data,
    by = join_key
  ) |>
    dplyr::mutate(output_type_id = NA) |>
    dplyr::rename(
      oracle_value = "observation"
    )

  return(oracle_output)
}

#' Generate and save oracle output for the Hub
#'
#' @param hub_path Path to the hub root.
#' @param output_dirpath Directory in which to
#' write the oracle-output.parquet file. Defaults
#' to the `target-data` subdirectory of the hub specified
#' in `hub_path`.
#' @param ts_date_col Name of the date column in
#' the `time-series` format target data. Will be converted
#' to `target_end_date` for the oracle output. Default `date`.
#'
#' @return nothing, invisibly, on success.
#' @export
write_oracle_output <- function(
  hub_path,
  output_dirpath = fs::path(hub_path, "target-data"),
  ts_date_col = "date"
) {
  fs::dir_create(output_dirpath)

  oracle_output <- generate_oracle_output_table(
    hub_path,
    ts_date_col = ts_date_col
  )

  output_file <- fs::path(output_dirpath, "oracle-output", ext = "parquet")
  forecasttools::write_tabular_file(oracle_output, output_file)
  return(invisible())
}

#' @rdname write_oracle_output
#'
#' @export
generate_oracle_output <- write_oracle_output
