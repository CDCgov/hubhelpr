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
#' @param model_output_dir Character. Name of the model
#' output directory. Defaults to "model-output".
#'
#' @return `NULL`, invisibly, raising an error if changes
#' are outside model-output or if the user is unauthorized.
#'
#' @export
check_changes_for_autoapproval <- function(
  changed_files,
  gh_actor,
  base_hub_path,
  model_output_dir = "model-output"
) {
  # validate inputs
  checkmate::assert_string(gh_actor)
  checkmate::assert_string(base_hub_path)
  checkmate::assert_string(model_output_dir)

  checkmate::assert_character(changed_files, min.len = 1)

  # tibble of changed files with paths
  changed_files_tbl <- tibble::tibble(
    full_path = changed_files
  ) |>
    dplyr::mutate(
      # normalize paths via remove leading slashes
      clean_path = stringr::str_remove(.data$full_path, "^/+"),
      # extract first directory from the path
      first_dir = stringr::str_extract(.data$clean_path, "^[^/]+"),
      # check file is in model-output
      in_model_output = .data$first_dir == model_output_dir,
      # extract model_id (second directory) for files in model-output
      model_id = dplyr::if_else(
        .data$in_model_output,
        stringr::str_extract(
          stringr::str_remove(
            .data$clean_path,
            glue::glue("^{model_output_dir}/")
          ),
          "^[^/]+"
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
        "Auto-approval failed: Changes detected outside '{model_output_dir}' directory.",
        "The following files are outside '{model_output_dir}':",
        files_outside_model_output
      )
    )
  }

  # extract unique model IDs
  changed_dirs <- changed_files_tbl |>
    dplyr::filter(!is.na(.data$model_id)) |>
    dplyr::pull(.data$model_id) |>
    unique()

  if (length(changed_dirs) == 0) {
    cli::cli_abort(
      "No valid model directories found in changed files."
    )
  }

  cli::cli_inform(
    "Checking authorization for {length(changed_dirs)} model director{?y/ies}: {.val {changed_dirs}}"
  )

  # pass model IDs to check_authorized_users
  check_authorized_users(
    changed_dirs = changed_dirs,
    gh_actor = gh_actor,
    base_hub_path = base_hub_path
  )

  invisible()
}
