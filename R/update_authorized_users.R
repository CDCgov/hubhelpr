#' Get authorized GitHub users from Hub model-metadata.
#'
#' This function reads model metadata from a Hub and extracts
#' authorized GitHub users for each model, saving the results
#' to a JSON file in the hub's `auxiliary-data` directory.
#' Models without authorized users will have an empty array
#' in the output.
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
      authorized_github_users = .data$designated_github_users |>
        na.omit() |>
        as.character() |>
        I() |>
        list(),
      .groups = "drop"
    )
  jsonlite::write_json(
    model_users,
    path = fs::path(output_dir, "authorized_users", ext = "json"),
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  invisible()
}
