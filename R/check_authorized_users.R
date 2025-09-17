#' Check if a user is authorized to modify model
#' directories. This function verifies whether a GitHub
#' user is authorized to modify specific model directories
#' in a Hub by checking the designated users in model
#' metadata.
#'
#' @param changed_dirs Character vector. Directory names
#' that have been changed.
#' @param gh_actor Character. GitHub username of the person
#' making changes.
#' @param base_hub_path Character. Path to the base hub
#' directory.
#'
#' @return NULL invisibly. Exits with error if the user is
#' unauthorized and prints success message if authorized.
#'
#' @export
check_authorized_users <- function(
  changed_dirs,
  gh_actor,
  base_hub_path
) {
  checkmate::assert_character(changed_dirs, min.len = 1)
  checkmate::assert_scalar(gh_actor)
  checkmate::assert_string(base_hub_path)

  model_metadata <- hubData::load_model_metadata(base_hub_path)

  if (nrow(model_metadata) == 0) {
    cli::cli_abort(
      "Error: Could not load model metadata from {base_hub_path}!"
    )
  }

  dir_users_map <- model_metadata |>
    dplyr::group_by(.data$model_id) |>
    dplyr::summarize(
      authorized_users = list(
        na.omit(.data$designated_github_users)
      ),
      .groups = "drop"
    )

  for (dir in changed_dirs) {
    if (!(dir %in% dir_users_map$model_id)) {
      cli::cli_abort(
        "Error: {dir} is not authorized for modification!"
      )
    }

    user_list <- dir_users_map |>
      dplyr::filter(.data$model_id == dir) |>
      dplyr::pull(.data$authorized_users) |>
      purrr::pluck(1)

    if (length(user_list) == 0) {
      cli::cli_abort(
        "Error: Changes found in '{dir}/', but no authorized users listed!"
      )
    }

    if (!(gh_actor %in% user_list)) {
      cli::cli_abort(
        paste0(
          "Error: Only the following users can modify '{dir}/': ",
          "{paste(user_list, collapse = ', ')}"
        )
      )
    }
  }

  cli::cli_inform(
    "Success: Changes authorized for user '{gh_actor}'."
  )
  invisible(NULL)
}
