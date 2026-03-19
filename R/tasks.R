#' Utilities for working with Hub modeling tasks

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


#' Get a vector of representative round ids for a modeling hub
#'
#' Wrapper of [hubUtils::get_round_ids()] that returns a
#' flat vector of round_id values that fully specify and uniquely index
#' the set of available task ids regardless of whether `round_id_from_variable: true`.
#'
#' Functions such as [hubValidations::expand_model_out_grid()] will create
#' duplicate values when `round_id_from_variable: true` if they are called on all
#' valid round_id values, since what we actually want to do is call them once
#' for each unique `round_idx`. But since [hubValidations::expand_model_out_grid()]
#' expects a round_id rather than a round_idx for input, the solution is to
#' compute a set of _representative_ `round id` values, one for each unique `round_idx`.
#' Note that when `round_id_from_variable: false`, this should simply by the set of
#' `round_id` values.
#'
#' @param config_tasks List of Hub tasks, as the
#' output of [hubUtils::read_config()] or the `config_tasks`
#' attribute of a [`<hub_connection>`][hubData::connect_hub()].
#' @return Representative round ids, as a vector.
#'
#' @examples
#'
#' hub_con <- hubData::connect_hub(
#'  system.file("testhubs/simple", package = "hubUtils")
#' )
#' config_tasks <- attr(hub_con, "config_tasks")
#'
#' # multiple round ids derived from `reference_date` values via
#' # `round_id_from_variable: true`
#' all_round_ids <- hubUtils::get_round_ids(config_tasks)
#' all_round_ids
#'
#' # But these all correspond to a single `round_idx`:
#' purrr::map_vec(all_round_ids, \(id) hubUtils::get_round_idx(config_tasks, id))
#'
#' # get representative round ids returns a single "representative" id
#' # corresponding to this single `round_idx`:
#' get_representative_round_ids(config_tasks)
#'
#' @export
get_representative_round_ids <- function(config_tasks) {
  ids <- get_round_ids_vec(config_tasks)
  rep_ids <- purrr::map_df(ids, \(id) {
    tibble::tibble(id = id, idx = hubUtils::get_round_idx(config_tasks, id))
  }) |>
    dplyr::distinct(.data$idx, .keep_all = TRUE) |>
    dplyr::pull(.data$id)

  return(rep_ids)
}


#' Get a `tibble` of all modeling tasks specified on the Hub.
#'
#' @param hub_path Path to the hub root.
#' @return A [`tibble`][tibble::tibble()] of modeling tasks,
#'
#' @examples
#'
#' get_hub_tasks(system.file("testhubs/v6/target_dir", package = "hubUtils"))
#'
#' @export
get_hub_tasks <- function(hub_path) {
  hub_con <- hubData::connect_hub(hub_path)
  config_tasks <- attr(hub_con, "config_tasks")

  rep_round_ids <- get_representative_round_ids(config_tasks)

  tasks <- purrr::map_df(rep_round_ids, \(id) {
    hubValidations::expand_model_out_grid(
      config_tasks,
      id,
      derived_task_ids = "target_end_date",
      required_vals_only = FALSE,
      force_output_types = FALSE
    ) |>
      dplyr::select(-"output_type_id") |>
      dplyr::distinct()
  })

  return(tasks)
}
