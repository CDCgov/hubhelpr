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

  model_metadata <- hubData::load_model_metadata(base_hub_path)

  json_list <- purrr::pmap(
    list(
      team = model_metadata$team_abbr,
      model = model_metadata$model_abbr,
      designated_users = model_metadata$designated_github_users
    ),
    \(team, model, designated_users) {
      team <- if (!is.null(team) && !is.na(team)) team else NA_character_
      model <- if (!is.null(model) && !is.na(model)) model else NA_character_
      designated_users <- if (
        !is.null(designated_users) && !all(is.na(designated_users))
      ) {
        # ensure single-user lists are rendered as JSON lists
        I(designated_users)
      } else {
        NA
      }
      list(
        model = paste(team, model, sep = "-"),
        authorized_github_users = designated_users
      )
    }
  )

  jsonlite::write_json(
    json_list,
    path = fs::path(output_dir, "authorized_users", ext = "json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  invisible()
}
