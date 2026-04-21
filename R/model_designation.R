#' Resolve per-target model designation.
#'
#' Annotates each (model_id, target) pair with whether
#' the model is designated for that target, per the
#' model's metadata fields `designated_model` and the
#' optional `designated_targets` list. If
#' `designated_model` absent or FALSE: never designated.
#' If `designated_model` TRUE and `designated_targets`
#' absent or empty: designated for every target supplied.
#' If `designated_model` TRUE and `designated_targets`
#' present: designated only for targets in that list.
#'
#' @param base_hub_path character, path to the base hub
#' directory.
#' @param model_target_pairs data frame with columns
#' `model_id` and `target` giving the (model, target)
#' pairs to resolve designation for. Typically the
#' distinct combinations present in forecast data.
#'
#' @return A tibble with columns `model_id`, `target`,
#' and `designated_model` (logical).
#' @export
get_model_designation <- function(base_hub_path, model_target_pairs) {
  checkmate::assert_names(
    colnames(model_target_pairs),
    must.include = c("model_id", "target"),
    .var.name = "model_target_pairs columns"
  )

  pairs <- model_target_pairs |>
    dplyr::select("model_id", "target") |>
    dplyr::distinct()

  metadata <- hubData::load_model_metadata(
    base_hub_path,
    model_ids = unique(pairs$model_id)
  ) |>
    dplyr::select(
      "model_id",
      "designated_model",
      dplyr::any_of("designated_targets")
    ) |>
    dplyr::distinct()

  if (!"designated_targets" %in% colnames(metadata)) {
    metadata <- metadata |>
      dplyr::mutate(designated_targets = NA_character_)
  } else {
    metadata <- metadata |>
      dplyr::mutate(
        designated_targets = as.character(.data$designated_targets)
      )
  }

  designated_all_targets <- metadata |>
    dplyr::filter(.data$designated_model, is.na(.data$designated_targets)) |>
    dplyr::distinct(.data$model_id)

  designated_specific_targets <- metadata |>
    dplyr::filter(.data$designated_model, !is.na(.data$designated_targets)) |>
    dplyr::distinct(.data$model_id, .data$designated_targets) |>
    dplyr::rename(target = "designated_targets")

  designated_pairs <- dplyr::bind_rows(
    designated_all_targets |>
      tidyr::crossing(target = unique(pairs$target)),
    designated_specific_targets
  ) |>
    dplyr::mutate(designated_model = TRUE)

  pairs |>
    dplyr::left_join(designated_pairs, by = c("model_id", "target")) |>
    dplyr::mutate(
      designated_model = dplyr::coalesce(.data$designated_model, FALSE)
    ) |>
    dplyr::select("model_id", "target", "designated_model")
}
