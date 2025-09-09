#' Get authorized GitHub users from Hub model-metadata.
#'
#' This function reads YAML metadata files from a Hub's
#' model-metadata directory and extracts authorized GitHub
# users for each model, saving the results to a JSON file in the hub `auxiliary-data` directory.
# JSON file.
#'
#' @param base_hub_path Path to the base hub directory.
#' @return `NULL`, invisibly. Writes `authorized_users.json` file to the
#' auxiliary-data directory.
update_authorized_users <- function(base_hub_path) {
  output_path <- fs::path(base_hub_path, "auxiliary-data")
  metadata_dir <- fs::path(base_hub_path, "model-metadata")

  fs::dir_create(output_path)

  yml_files <- list.files(
    metadata_dir,
    pattern = "\\.ya?ml$",
    full.names = TRUE
  )

  json_list <- purrr::map(yml_files, \(file) {
    y <- yaml::read_yaml(file)
    team <- if (!is.null(y$team_abbr)) y$team_abbr else NA_character_
    model <- if (!is.null(y$model_abbr)) y$model_abbr else NA_character_
    designated_users <- if (!is.null(y$designated_github_users)) {
      # ensure single-user lists are rendered as JSON lists
      I(y$designated_github_users)
    } else {
      NA
    }
    list(
      model = paste(team, model, sep = "-"),
      authorized_github_users = designated_users
    )
  })

  jsonlite::write_json(
    json_list,
    path = file.path(output_path, "authorized_users.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  invisible()
}
