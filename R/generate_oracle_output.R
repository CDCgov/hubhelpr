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

  config_tasks <- hubUtils::read_config(hub_path, "tasks")
  round_ids <- hubUtils::get_round_ids(config_tasks)

  ## this involves duplication given how hubUtils::get_round_model_tasks
  ## behaves by default with round ids created from reference dates,
  ## but to support hubs with round_ids created in other ways, we
  ## do it this way and then deduplicate as needed.
  list_of_task_lists <- purrr::map(round_ids, \(id) {
    hubUtils::get_round_model_tasks(config_tasks, id)
  })

  unique_tasks <- purrr::map_df(list_of_task_lists, flatten_task_list) |>
    dplyr::distinct() |>
    dplyr::mutate(target_end_date = as.Date(.data$target_end_date))

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
