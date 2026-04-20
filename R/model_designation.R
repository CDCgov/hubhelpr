#' Read model designation fields from hub metadata.
#'
#' Reads each model's metadata YAML and returns a
#' tibble of designation fields (`designated_model` and
#' the optional `designated_targets`).
#'
#' @param base_hub_path character, path to the base hub
#' directory.
#' @param model_ids character vector of model IDs to
#' read (or NULL to read all models). Default: NULL.
#'
#' @return A tibble with columns `model_id` (character),
#' `designated_model` (logical), and `designated_targets`
#' (list-column of character vectors; NULL for models
#' that do not designation).
#' @noRd
read_model_designation <- function(base_hub_path, model_ids = NULL) {
  metadata_dir <- fs::path(base_hub_path, "model-metadata")
  if (!fs::dir_exists(metadata_dir)) {
    cli::cli_abort(
      "{.path model-metadata} directory not found at {.path {metadata_dir}}."
    )
  }

  metadata_paths <- fs::dir_ls(metadata_dir, glob = "*.y*ml")
  metadata_ids <- fs::path_ext_remove(basename(metadata_paths))

  if (!is.null(model_ids)) {
    keep <- metadata_ids %in% model_ids
    metadata_paths <- metadata_paths[keep]
    metadata_ids <- metadata_ids[keep]
  }

  tibble::tibble(
    model_id = metadata_ids,
    raw = purrr::map(metadata_paths, yaml::read_yaml)
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      designated_model = isTRUE(.data$raw[["designated_model"]]),
      designated_targets = list(.data$raw[["designated_targets"]])
    ) |>
    dplyr::ungroup() |>
    dplyr::select("model_id", "designated_model", "designated_targets")
}


#' Decide whether a single (model, target) pair is
#' designated.
#'
#' @param designated_model logical scalar, the model's
#' `designated_model` flag.
#' @param target character scalar, the target name.
#' @param designated_targets character vector or NULL,
#' the model's `designated_targets` narrowing list.
#'
#' @return Logical scalar; TRUE iff model is designated
#' for given target.
#' @noRd
is_designated_for_target <- function(
  designated_model,
  target,
  designated_targets
) {
  if (!isTRUE(designated_model)) {
    return(FALSE)
  }
  if (is.null(designated_targets) || length(designated_targets) == 0L) {
    return(TRUE)
  }
  target %in% designated_targets
}


#' Resolve per-target model designation.
#'
#' Annotates each (model_id, target) pair with whether
#' the model is designated for that target, per the
#' model's metadata fields `designated_model` and the
#' optional `designated_targets` list. If
#' `designated_model` absent or FALSE: never designated.
#' If`designated_model` TRUE and `designated_targets`
#' absent or empty: designated for every target supplied.
#' If `designated_model` TRUE  and `designated_targets`
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

  designated_targets <- read_model_designation(
    base_hub_path,
    model_ids = unique(pairs$model_id)
  ) |>
    assert_known_designated_targets(unique(pairs$target))

  pairs |>
    dplyr::left_join(designated_targets, by = "model_id") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      designated_model = is_designated_for_target(
        .data$designated_model,
        .data$target,
        .data$designated_targets
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select("model_id", "target", "designated_model")
}


#' Assert all designated_targets entries are known.
#'
#' Returns `designation` unchanged on success, aborts
#' if any model's `designated_targets` lists target
#' not present in `known_targets`.
#'
#' @param designation tibble with columns `model_id`,
#' `designated_model`, and `designated_targets`
#' (list-column), as returned by `read_model_designation()`.
#' @param known_targets character vector of target names
#' present in the caller's (model_id, target) grid.
#'
#' @return `designation`, invisibly.
#' @noRd
assert_known_designated_targets <- function(
  designation,
  known_targets
) {
  flagged <- designation |>
    dplyr::filter(lengths(.data$designated_targets) > 0L) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      unknown = list(setdiff(.data$designated_targets, known_targets))
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(lengths(.data$unknown) > 0L)

  if (nrow(flagged) > 0L) {
    cli::cli_abort(c(
      "Model metadata references targets not present in the forecast data.",
      "i" = "Check {.field designated_targets} in the metadata for: {.val {flagged$model_id}}."
    ))
  }

  invisible(designation)
}
