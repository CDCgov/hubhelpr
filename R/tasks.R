#' Utilities for working with Hub modeling tasks

#' Transform a modeling task represented as a nested
#' list to a single data frame.
#'
#' @param task Nested list representing a modeling task,
#' as one entry of the output of
#' [hubUtils::get_round_model_tasks()]. Must have a
#' `target_end_date` specification.
#' @return A [`tibble`][tibble::tibble()] of all
#' potentially valid submittable outputs for the
#' modeling task defined in `task`. Each row of the
#' table represents a single valid forecastable quantity
#' (e.g. "`target` X on `target_end_date` Y in `location`
#' Z"), plus a valid submittable output_type for
#' forecasting that quantity. If multiple `output_type`s
#' are accepted for a given valid forecastable quantity,
#' that quantity will be represented multiple times,
#' with one row for each valid associated `output_type`.
#' @export
flatten_task <- function(task) {
  checkmate::assert_names(
    names(task),
    must.include = c("output_type", "task_ids")
  )
  checkmate::assert_names(
    names(task$task_ids),
    must.include = "target_end_date"
  )
  output_types <- names(task$output_type)

  task_params <- purrr::map(task$task_ids, \(x) c(x$required, x$optional)) |>
    purrr::discard_at(c("horizon", "reference_date"))
  ## discard columns that are redundant with `target_end_date`

  return(do.call(
    tidyr::crossing,
    c(task_params, list(output_type = output_types))
  ))
}


#' Transform a group of modeling tasks represented as a
#' list of nested lists into a single data frame.
#'
#' Calls `flatten_task()` on each entry of the task list.
#'
#' @param task_list List of tasks. Each entry should
#' itself be a nested list that can be passed to
#' `flatten_task()`.
#' @param .deduplicate deduplicate the output if the
#' same flat configuration is found multiple times
#' while flattening the task list? Default `TRUE`.
#'
#' @return A [`tibble`][tibble::tibble()] of all
#' potentially valid submittable outputs for all the
#' modeling tasks defined in `task_list`. Each row of
#' the table represents a single valid forecastable
#' quantity (e.g. "`target` X on `target_end_date` Y in
#' `location` Z"), plus a valid submittable output_type
#' for forecasting that quantity. If multiple
#' `output_type`s are accepted for a given valid
#' forecastable quantity, that quantity will be
#' represented multiple times, with one row for each
#' valid associated `output_type`.
#' @export
flatten_task_list <- function(task_list, .deduplicate = TRUE) {
  flat_tasks <- purrr::map_df(task_list, flatten_task)

  if (.deduplicate) {
    flat_tasks <- dplyr::distinct(flat_tasks)
  }

  return(flat_tasks)
}

#' Get a `tibble` of all modeling tasks specified on the Hub.
#'
#' By default, deduplicates the [`tibble`][tibble::tibble()]
#' using [dplyr::distinct()].
#'
#' @param hub_path Path to the hub root.
#' @param .deduplicate deduplicate the resulting
#' [`tibble`][tibble::tibble()] using [dplyr::distinct()]?
#' Default `TRUE`.
#' @return A [`tibble`][tibble::tibble()] of modeling tasks.
#'
#' @export
get_hub_tasks <- function(hub_path, .deduplicate = TRUE) {
  config_tasks <- hubUtils::read_config(hub_path, "tasks")
  round_ids <- hubUtils::get_round_ids(config_tasks)

  ## this involves duplication given how hubUtils::get_round_model_tasks
  ## behaves by default with round ids created from reference dates,
  ## but to support hubs with round_ids created in other ways, we
  ## do it this way and then deduplicate as needed.

  tasks <- purrr::map(round_ids, \(id) {
      hubUtils::get_round_model_tasks(config_tasks, id)
  }) |>
      purrr::map_df(flatten_task_list) |>
      dplyr::mutate(dplyr::across(
                               c("reference_date", "target_end_date"),
                               as.Date))

  if(.deduplicate) {
      tasks <- tasks |> dplyr::distinct()
  }

  return(tasks)
}
