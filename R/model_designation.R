# Submission records written before this date have no
# `target` column, because hospital admissions was the only
# target the hub carried: proportion of ED visits enters
# covid19-forecast-hub's tasks.json for reference date
# 2025-06-21, and the column follows on 2025-07-05. A record
# missing the column after this date is malformed, not old.
last_date_without_designation_by_target <- lubridate::as_date("2025-06-28")


#' Path to the hub's record of which models submitted
#' for a reference date.
#'
#' [generate_hub_ensemble()] writes
#' `auxiliary-data/weekly-model-submissions/` each week,
#' recording the models that submitted and how they were
#' designated at the time. That record is the only source
#' of designation for a past reference date:
#' `model-metadata/` describes models as they are now,
#' and designation changes as models come and go.
#'
#' @param base_hub_path character, path to the base hub
#' directory.
#' @param reference_date character or Date, the
#' reference date whose record to locate.
#'
#' @return The file path, whether or not it exists.
#' @noRd
model_submissions_path <- function(base_hub_path, reference_date) {
  return(fs::path(
    base_hub_path,
    "auxiliary-data",
    "weekly-model-submissions",
    glue::glue("{lubridate::as_date(reference_date)}-models-submitted-to-hub"),
    ext = "csv"
  ))
}


#' Read the hub's record of which models submitted for
#' a reference date.
#'
#' @inheritParams model_submissions_path
#'
#' @return A tibble with columns `model_id` and
#' `designated`, plus `target` when the record has one.
#' @noRd
read_weekly_model_submissions <- function(
  base_hub_path,
  reference_date
) {
  submissions_path <- model_submissions_path(base_hub_path, reference_date)

  submissions <- readr::read_csv(submissions_path, show_col_types = FALSE) |>
    dplyr::rename_with(tolower) |>
    dplyr::rename(
      model_id = tidyselect::any_of(c("model", "model_id")),
      designated = tidyselect::any_of(c("designated_model", "designated"))
    )

  checkmate::assert_names(
    names(submissions),
    must.include = c("model_id", "designated")
  )

  return(
    submissions |>
      dplyr::mutate(designated = as.logical(.data$designated)) |>
      dplyr::select(
        "model_id",
        tidyselect::any_of("target"),
        "designated"
      )
  )
}


#' Resolve per-target model designation as it stands
#' now.
#'
#' Returns full grid of (model_id, target, designated)
#' for the requested models and targets, resolved from
#' current model metadata: the fields `designated_model`
#' and the optional `designated_targets` list. If
#' `designated_model` is FALSE: never designated. If
#' `designated_model` is TRUE and `designated_targets`
#' is absent or empty: designated for every target. If
#' `designated_model` is TRUE and `designated_targets`
#' is present: designated only for listed targets.
#'
#' This describes models as they are today. This is
#' only correct for the current reference date. Use
#' [get_model_designation_as_of()] for a past one. The
#' exception is [generate_hub_ensemble()], which writes
#' the record the as-of version reads.
#'
#' @param base_hub_path character, path to the base hub
#' directory.
#' @param model_ids character vector of model IDs to
#' include, or NULL (default) to include all models
#' with metadata in the hub.
#' @param targets character vector of target names to
#' include, or NULL (default) to include all targets
#' supported by the hub.
#'
#' @return A tibble with columns `model_id`, `target`,
#' and `designated` (logical), with one row per
#' (model, target) combination.
#' @export
get_model_designation_current <- function(
  base_hub_path,
  model_ids = NULL,
  targets = NULL
) {
  if (is.null(targets)) {
    targets <- get_hub_supported_targets(base_hub_path)
  }

  metadata <- hubData::load_model_metadata(
    base_hub_path,
    model_ids = model_ids
  ) |>
    dplyr::select(
      "model_id",
      "designated_model",
      dplyr::any_of("designated_targets")
    )

  if (!"designated_targets" %in% colnames(metadata)) {
    metadata <- metadata |>
      dplyr::mutate(designated_targets = list(targets))
  } else {
    metadata <- metadata |>
      dplyr::mutate(
        designated_targets = purrr::map(
          .data$designated_targets,
          ~ if (length(.x) == 0L) targets else as.character(.x)
        )
      )
  }

  metadata |>
    tidyr::crossing(target = targets) |>
    dplyr::mutate(
      designated = .data$designated_model &
        purrr::map2_lgl(
          .data$target,
          .data$designated_targets,
          `%in%`
        )
    ) |>
    dplyr::select("model_id", "target", "designated")
}


#' Resolve per-target model designation as of a
#' reference date.
#'
#' Reads the hub's record of designation statuses used
#' to generate the ensemble for that reference date
#' (via [generate_hub_ensemble()]), not the current
#' designation statuses.
#'
#' @param base_hub_path character, path to the base hub
#' directory.
#' @param reference_date character or Date, the
#' reference date to resolve designation as of.
#' @param model_ids character vector of model IDs to
#' include, or NULL (default) to include every model in
#' that reference date's record
#' @param targets character vector of target names to
#' include, or NULL (default) to include all targets
#' supported by the hub.
#'
#' @return A tibble with columns `model_id`, `target`,
#' and `designated` (logical), with one row per
#' (model, target) combination. `model_id`s with no
#' submission for the reference date have an `NA`
#' designation status.
#' @export
get_model_designation_as_of <- function(
  base_hub_path,
  reference_date,
  model_ids = NULL,
  targets = NULL
) {
  if (is.null(targets)) {
    targets <- get_hub_supported_targets(base_hub_path)
  }

  submissions_path <- model_submissions_path(base_hub_path, reference_date)

  if (!fs::file_exists(submissions_path)) {
    cli::cli_abort(
      c(
        "No submission record for {.val {as.character(reference_date)}}
         at {.path {submissions_path}}.",
        "i" = "Model designation statuses are recorded when the ensemble
               is built for a given reference date."
      )
    )
  }

  submissions <- read_weekly_model_submissions(
    base_hub_path,
    reference_date
  )

  if (!("target" %in% names(submissions))) {
    if (
      lubridate::as_date(reference_date) >
        last_date_without_designation_by_target
    ) {
      cli::cli_abort(
        c(
          "The submission record for
           {.val {as.character(reference_date)}} has no {.field target}
           column.",
          "i" = "Records have carried one since
                 {.val {as.character(last_date_without_designation_by_target + 1)}}.
                 A missing column is only meaningful for earlier dates,
                 when hospital admissions was the hub's only target."
        )
      )
    }
    # before designation-by-target was introduced, a
    # model's designation status applied across all targets
    submissions <- tidyr::crossing(submissions, target = targets)
  }

  return(
    tidyr::crossing(
      model_id = model_ids %||% unique(submissions$model_id),
      target = targets
    ) |>
      dplyr::left_join(submissions, by = c("model_id", "target"))
  )
}


#' Count designated models per reference_date, target,
#' location, and horizon.
#'
#' Gets the number of designated models that
#' contributed forecasts for each combination of target,
#' location, reference date, and horizon; useful for
#' checking minimum model thresholds for ensemble
#' reporting.
#'
#' @param base_hub_path character, path to the base hub
#' directory.
#' @param reference_dates character or Date vector of
#' reference dates in YYYY-MM-DD format. If NULL
#' (default), includes all available reference dates.
#' @param targets character vector of target names to
#' include. If NULL (default), includes all supported
#' targets.
#' @param horizons integer vector of horizons to include.
#' If NULL (default), includes all available horizons.
#' @param output_types character vector of output types
#' to include. Default: NULL (include all available
#' output types).
#'
#' @return A tibble with columns `reference_date`,
#' `target`, `location`, `horizon`, `output_type`,
#' and `n_models`.
#'
#' @export
count_designated_models <- function(
  base_hub_path,
  reference_dates = NULL,
  targets = NULL,
  horizons = NULL,
  output_types = NULL
) {
  hub_forecasts <- hubData::connect_hub(base_hub_path)

  if (!is.null(reference_dates)) {
    reference_dates <- lubridate::as_date(reference_dates)
    hub_forecasts <- hub_forecasts |>
      dplyr::filter(.data$reference_date %in% !!reference_dates)
  }

  hub_forecasts <- hubData::collect_hub(hub_forecasts)

  if (nrow(hub_forecasts) == 0) {
    cli::cli_abort(
      "No forecast data found for the specified reference date(s)."
    )
  }

  designated_pairs <- hub_forecasts |>
    dplyr::distinct(.data$reference_date, .data$model_id) |>
    tidyr::nest(.by = "reference_date", .key = "models") |>
    purrr::pmap(\(reference_date, models) {
      get_model_designation_as_of(
        base_hub_path,
        reference_date = reference_date,
        model_ids = models$model_id
      ) |>
        dplyr::filter(.data$designated) |>
        dplyr::mutate(reference_date = !!reference_date) |>
        dplyr::select("reference_date", "model_id", "target")
    }) |>
    purrr::list_rbind()

  designated_forecasts <- hub_forecasts |>
    dplyr::inner_join(
      designated_pairs,
      by = c("reference_date", "model_id", "target")
    ) |>
    dplyr::filter(
      forecasttools::nullable_comparison(.data$target, "%in%", !!targets),
      forecasttools::nullable_comparison(.data$horizon, "%in%", !!horizons),
      forecasttools::nullable_comparison(
        .data$output_type,
        "%in%",
        !!output_types
      )
    )

  hub_task_grid <- get_hub_tasks(base_hub_path) |>
    dplyr::distinct(
      .data$reference_date,
      .data$target,
      .data$location,
      .data$horizon,
      .data$output_type
    ) |>
    dplyr::filter(
      forecasttools::nullable_comparison(
        .data$reference_date,
        "%in%",
        !!reference_dates
      ),
      forecasttools::nullable_comparison(.data$target, "%in%", !!targets),
      forecasttools::nullable_comparison(.data$horizon, "%in%", !!horizons),
      forecasttools::nullable_comparison(
        .data$output_type,
        "%in%",
        !!output_types
      )
    )

  designated_counts <- designated_forecasts |>
    dplyr::summarise(
      n_models = dplyr::n_distinct(.data$model_id),
      .by = c("reference_date", "target", "location", "horizon", "output_type")
    )

  hub_task_grid |>
    dplyr::left_join(
      designated_counts,
      by = c("reference_date", "target", "location", "horizon", "output_type")
    ) |>
    dplyr::mutate(
      n_models = dplyr::coalesce(.data$n_models, 0L)
    )
}
