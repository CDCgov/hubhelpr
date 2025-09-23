#' Check if a user is authorized to modify model
#' directories.
#'
#' This function verifies whether a GitHub
#' user is authorized to modify specific directories
#' in a Hub by checking the designated users in model
#' metadata.
#'
#' @param changed_dirs Character vector. Full paths of files
#' that have been modified.
#' @param gh_actor Character. GitHub username of the person
#' making changes.
#' @param base_hub_path Character. Path to the base hub
#' directory.
#'
#' @return `NULL`, invisibly, raising an error if the user is
#' unauthorized and printing a success message if the user is authorized.
#'
#' @export
check_authorized_users <- function(
  changed_dirs,
  gh_actor,
  base_hub_path
) {
  checkmate::assert_character(changed_dirs, min.len = 1)
  checkmate::assert_string(gh_actor)
  checkmate::assert_string(base_hub_path)

  model_metadata <- hubData::load_model_metadata(base_hub_path) |>
    dplyr::mutate(is_model_dir = TRUE) |>
    dplyr::rename(dir = "model_id")

  # process full paths to extract model directories
  changed_dirs_tbl <- tibble::tibble(full_path = changed_dirs) |>
    dplyr::mutate(
      # check if change is within model-output directory
      is_model_output_change = grepl(
        "model-output/",
        .data$full_path,
        fixed = TRUE
      )
    )

  # check for changes outside model-output
  outside_changes <- changed_dirs_tbl |>
    dplyr::filter(!.data$is_model_output_change)

  if (nrow(outside_changes) > 0) {
    cli::cli_abort(
      c(
        "Changes found outside model-output directory are not allowed in auto-approved PRs:",
        outside_changes$full_path
      )
    )
  }

  # extract model directory names from model-output paths
  changed_dirs_tbl <- changed_dirs_tbl |>
    dplyr::filter(.data$is_model_output_change) |>
    dplyr::mutate(
      # extract model directory using fs path functions
      dir = fs::path_file(fs::path_dir(fs::path_rel(
        .data$full_path,
        "model-output"
      )))
    ) |>
    dplyr::select(.data$dir) |>
    dplyr::distinct()

  # if no model-output changes, nothing to check
  if (nrow(changed_dirs_tbl) == 0) {
    cli::cli_inform("No model-output changes found.")
    return(invisible())
  }

  authorization_check <- changed_dirs_tbl |>
    dplyr::left_join(model_metadata, by = "dir", na_matches = "never") |>
    dplyr::group_by(.data$dir) |>
    dplyr::summarize(
      modifiable = all(tidyr::replace_na(.data$is_model_dir, FALSE)),
      actor_authorized = !!gh_actor %in% .data$designated_github_users,
      authorized_users = paste(
        na.omit(.data$designated_github_users),
        collapse = ", "
      ),
      has_authorized_users = length(na.omit(.data$designated_github_users)) > 0,
      .groups = "drop"
    )

  problem_dirs <- authorization_check |>
    dplyr::filter(!.data$actor_authorized)

  if (nrow(problem_dirs) > 0) {
    error_messages <- problem_dirs |>
      dplyr::mutate(
        error_msg = dplyr::case_when(
          !.data$modifiable ~
            glue::glue(
              "'{.data$dir}' cannot be modified in auto-approved PRs.",
              "If this is your team's model output subdirectory, check ",
              "that the Hub already has a model metadata file for this model."
            ),
          !.data$has_authorized_users ~
            glue::glue(
              "Changes found in model directory '{.data$dir}/' but no authorized users listed for that model."
            ),
          .data$has_authorized_users ~
            glue::glue(
              "Only the following users can modify the model directory '{.data$dir}/': {.data$authorized_users}"
            ),
          TRUE ~
            "Encountered an unexpected error while checking change authorization."
        )
      ) |>
      dplyr::pull(.data$error_msg)

    cli::cli_abort(
      c(
        "Authorization check failed for user '{gh_actor}':",
        error_messages
      )
    )
  }

  cli::cli_inform(
    "Success: Changes authorized for user '{gh_actor}'."
  )
  invisible()
}
