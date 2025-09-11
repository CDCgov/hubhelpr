#' Get authorized GitHub users from Hub model-metadata.
#'
#' This function reads YAML metadata files from a Hub's
#' model-metadata directory and extracts authorized GitHub
#' users for each model, saving the results to a JSON file
#' in the hub `auxiliary-data` directory.
#' JSON file.
#'
#' @param base_hub_path Path to the base hub directory.
#' @return `NULL`, invisibly. Writes
#' `authorized_users.json` file to the `auxiliary-data`
#' directory of the hub as a side effect.
#' @export
update_authorized_users <- function(base_hub_path) {
  output_dir <- fs::path(base_hub_path, "auxiliary-data")

  fs::dir_create(output_dir)

  model_users <- hubData::load_model_metadata(base_hub_path) |>
    dplyr::group_by(.data$model_id) |>
    dplyr::summarize(
      authorized_github_users = if (
        any(!is.na(.data$designated_github_users))
      ) {
        I(list(.data$designated_github_users[
          !is.na(.data$designated_github_users)
        ]))
      } else {
        NA
      },
      .groups = "drop"
    )

  json_list <- model_users |>
    dplyr::select("model_id", "authorized_github_users") |>
    purrr::pmap(\(model_id, authorized_github_users) {
      list(
        model = model_id,
        authorized_github_users = authorized_github_users
      )
    })

  jsonlite::write_json(
    json_list,
    path = fs::path(output_dir, "authorized_users", ext = "json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  invisible()
}
