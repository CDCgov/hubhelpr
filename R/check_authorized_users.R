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
#' @return Exits invisibly, with error if the user is
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

  dir_users_map <- model_metadata |>
    dplyr::group_by(.data$model_id) |>
    dplyr::summarize(
      authorized_users = list(
        as.character(na.omit(.data$designated_github_users))
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(dir = "model_id")

  changed_dirs_tbl <- tibble::tibble(dir = changed_dirs)

  authorization_check <- changed_dirs_tbl |>
    dplyr::left_join(dir_users_map, by = "dir") |>
    dplyr::mutate(
      dir_not_modifiable = purrr::map_lgl(.data$authorized_users, is.null),
      has_authorized_users = purrr::map_lgl(
        .data$authorized_users,
        \(authorized) length(authorized) > 0
      ),
      actor_authorized = purrr::map_lgl(
        .data$authorized_users,
        \(authorized) gh_actor %in% authorized
      ),
    )

  problem_dirs <- authorization_check |>
    dplyr::filter(!.data$actor_authorized)

  if (nrow(problem_dirs) > 0) {
    error_messages <- problem_dirs |>
      dplyr::mutate(
        error_msg = dplyr::case_when(
          .data$dir_not_modifiable ~
            paste0(
              "'",
              .data$dir,
              "' is not authorized for modification."
            ),
          !.data$has_authorized_users ~
            paste0(
              "Changes found in '",
              .data$dir,
              "/'; no authorized users listed."
            ),
          TRUE ~
            paste0(
              "Only the following users can modify: '",
              .data$dir,
              "/': ",
              purrr::map_chr(
                .data$authorized_users,
                function(x) paste(x, collapse = ", ")
              )
            )
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
