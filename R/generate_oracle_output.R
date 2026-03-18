#' Generate and save oracle output for the Hub
#'
#' @param hub_path Path to the hub root.
#'
#' @return nothing, invisibly, on success.
#' @export
generate_oracle_output <- function(hub_path) {
  output_dirpath <- fs::path(hub_path, "target-data")
  fs::dir_create(output_dirpath)
  target_ts <- hubData::connect_target_timeseries(hub_path)

  unique_tasks <- get_hub_tasks(hub_path, .deduplicate = TRUE)

  target_data <- target_ts |>
    forecasttools::hub_target_data_as_of("latest", .drop = TRUE) |>
    dplyr::collect() |>
    dplyr::rename(target_end_date = "date")

  join_key <- intersect(
    colnames(unique_tasks),
    colnames(target_data)
  )

  oracle_data <- dplyr::inner_join(unique_tasks, target_data, by = join_key) |>
    dplyr::mutate(output_type_id = NA) |>
    dplyr::rename(
      oracle_value = "observation"
    )

  output_file <- fs::path(output_dirpath, "oracle-output", ext = "parquet")
  forecasttools::write_tabular_file(oracle_data, output_file)
  return(invisible())
}
