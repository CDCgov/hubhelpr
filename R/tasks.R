# Utilities for working with Hub modeling tasks

#' Get a vector of unique round ids for a modeling hub
#'
#' Wrapper of [hubUtils::get_round_ids()] that returns a
#' flat vector of unique round_id values regardless of whether
#' `round_id_from_variable` is `TRUE` or `FALSE`.
#'
#' [hubUtils::get_round_ids()] with `flatten = "all"` returns a vector,
#' but may duplicate round_ids when `round_id_from_variable: true`.
#' [hubUtils::get_round_ids()] with `flatten = "model_task"` handles
#' both `round_id_from_variable: true` and `round_id_from_variable: false`,
#' but returns a list. This function provides a workaround by calling
#' [purrr::list_c()] on the output of [hubUtils::get_round_ids()] with
#' `flatten = "model_task"`.
#'
#' @param config_tasks List of Hub tasks, as the
#' output of [hubUtils::read_config()] or the `config_tasks`
#' attribute of a [`<hub_connection>`][hubData::connect_hub()].
#' @return Unique round ids, as a vector.
#'
#' @examples
#' hub_con <- hubData::connect_hub(
#'  system.file("testhubs/simple", package = "hubUtils")
#' )
#' config_tasks <- attr(hub_con, "config_tasks")
#'
#' get_round_ids_vec(config_tasks)
#'
#' @export
get_round_ids_vec <- function(config_tasks) {
  return(purrr::list_c(hubUtils::get_round_ids(config_tasks, "model_task")))
}


#' Get a `tibble` of all modeling tasks specified on the Hub.
#'
#' @param hub_path Path to the hub root.
#' @param ... additional arguments passed to
#' [hubValidations::expand_model_out_grid()].
#' @return A [`tibble`][tibble::tibble()] of modeling tasks,
#'
#' @examples
#'
#' get_hub_tasks(system.file("testhubs/v6/target_dir", package = "hubUtils"))
#'
#' @export
get_hub_tasks <- function(hub_path, ...) {
  hub_con <- hubData::connect_hub(hub_path)
  config_tasks <- attr(hub_con, "config_tasks")

  round_ids <- get_round_ids_vec(config_tasks)

  tasks <- purrr::map_df(round_ids, \(id) {
    hubValidations::expand_model_out_grid(
      config_tasks,
      id,
      derived_task_ids = "target_end_date",
      ## hard-coded for now for the hubs we run,
      ## can be relaxed when
      ## https://github.com/CDCgov/covid19-forecast-hub/issues/1413
      ## https://github.com/CDCgov/rsv-forecast-hub/issues/392
      ## are addressed
      required_vals_only = FALSE,
      force_output_types = FALSE,
      ...
    ) |>
      dplyr::select(-"output_type_id") |>
      dplyr::distinct()
  })

  return(tasks)
}
