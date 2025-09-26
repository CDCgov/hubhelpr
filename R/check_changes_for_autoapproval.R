#' Check changed files for auto-approval eligibility.
#'
#' This function processes a list of changed files from a
#' GitHub workflow, errors on any changes outside
#' model-output directory, and passes model IDs to
#' check_authorized_users for authorization validation.
#'
#' @param changed_files Character vector. List of changed
#' file paths from the GitHub changed-files workflow output.
#' @param gh_actor Character. GitHub username of the
#' person making changes.
#' @param base_hub_path Character. Path to the base hub
#' directory.
#'
#' @return `NULL`, invisibly, raising an error if changes
#' are outside model-output or if the user is unauthorized.
#'
#' @export
check_changes_for_autoapproval <- function(
  changed_files,
  gh_actor,
  base_hub_path
) {
  # validate inputs
  checkmate::assert_string(gh_actor)
  checkmate::assert_string(base_hub_path)
  if (length(changed_files) == 0) {
    cli::cli_abort("Empty PRs cannot be autoapproved.")
  }
  checkmate::assert_character(changed_files)

  # tibble of changed files with paths
  changed_files_tbl <- tibble::tibble(
    full_path = changed_files
  ) |>
    dplyr::mutate(
      # normalize paths
      clean_path = fs::path_norm(.data$full_path),
      # get path parts
      path_parts = purrr::map(.data$clean_path, fs::path_split),
      # extract first directory from the path
      first_dir = purrr::map_chr(.data$path_parts, ~ .x[[1]][1]),
      # check file is in model-output
      in_model_output = .data$first_dir == "model-output",
      # extract model_id for files in model-output
      model_id = dplyr::if_else(
        .data$in_model_output,
        purrr::map_chr(
          .data$path_parts,
          ~ purrr::pluck(.x, 1, 2, .default = NA_character_)
        ),
        NA_character_
      )
    )

  # check for files outside model-output
  files_outside_model_output <- changed_files_tbl |>
    dplyr::filter(!.data$in_model_output) |>
    dplyr::pull(.data$full_path)

  if (length(files_outside_model_output) > 0) {
    cli::cli_abort(
      c(
        "Auto-approval failed: Changes detected outside 'model-output' directory.",
        "The following files are outside 'model-output':",
        files_outside_model_output
      )
    )
  }

  # extract unique model IDs
  changed_dirs <- changed_files_tbl |>
    dplyr::filter(.data$in_model_output) |>
    dplyr::pull(.data$model_id) |>
    unique()

  # only check authorization if there are model directories
  if (length(changed_dirs) > 0) {
    cli::cli_inform(
      "Checking authorization for {length(changed_dirs)} model director{?y/ies}: {.val {changed_dirs}}"
    )

    # pass model IDs to check_authorized_users
    check_authorized_users(
      changed_dirs = changed_dirs,
      gh_actor = gh_actor,
      base_hub_path = base_hub_path
    )
  }

  invisible()
}
