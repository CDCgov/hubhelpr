#' Normalize excluded locations to a named list.
#'
#' Converts a character vector or named list of excluded
#' locations into a consistent named list format.
#'
#' @param excluded_locations NULL, character vector, or
#' named list of character vector.
#'
#' @return Named list of character vectors.
#' @noRd
normalize_excluded_locations <- function(excluded_locations) {
  if (is.null(excluded_locations)) {
    return(list())
  }
  if (is.character(excluded_locations)) {
    return(list("all" = excluded_locations))
  }
  if (is.list(excluded_locations)) {
    return(excluded_locations)
  }
  cli::cli_abort(
    "{.arg excluded_locations} must be NULL, a character vector, or a named list."
  )
}


#' Build a target-location exclusion data frame.
#'
#' Constructs a tibble of target/location pairs to
#' exclude. Entries keyed by "all" are expanded into
#' one row per available target. Errors if any named
#' targets in the exclusion list are not in
#' `available_targets`.
#'
#' @param excluded_locations Named list as returned by
#' `normalize_excluded_locations()`.
#' @param available_targets character vector of valid
#' target names.
#'
#' @return A tibble with columns "target" and "location"
#' (hub codes).
#' @noRd
build_exclusion_df <- function(excluded_locations, available_targets) {
  named_targets <- setdiff(names(excluded_locations), "all")
  invalid_targets <- setdiff(named_targets, available_targets)
  if (length(invalid_targets) > 0) {
    cli::cli_abort(
      "{.arg excluded_locations} contains unknown target{?s}: {.val {invalid_targets}}."
    )
  }

  global_locs <- excluded_locations[["all"]] %||% character(0)
  per_target <- excluded_locations[named_targets]

  merged <- lapply(
    stats::setNames(available_targets, available_targets),
    \(tgt) unique(c(global_locs, per_target[[tgt]] %||% character(0)))
  )

  tibble::enframe(merged, name = "target", value = "location") |>
    tidyr::unnest(cols = "location") |>
    dplyr::mutate(
      location = forecasttools::us_location_recode(
        .data$location,
        "abbr",
        "hub"
      )
    )
}


#' Summarize forecast hub data for a specific reference date.
#'
#' This function generates a tibble of forecast data
#' for a given reference date. It can filter by model IDs;
#' allows flexibility retrieve all models or specific
#' subsets (e.g., ensemble only).
#'
#' @param reference_date character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param base_hub_path character, path to the forecast hub
#' directory.
#' @param disease character, disease name ("covid" or
#' "rsv"). Used to derive hub name and file prefix.
#' @param population_data data frame with columns "location"
#' and "population". Adds population-based calculations.
#' @param horizons_to_include integer vector, horizons to
#' include in the output. Default: c(0, 1, 2).
#' @param excluded_locations character vector or named list
#' specifying US state abbreviations to exclude. If a
#' character vector, locations are excluded across all
#' targets. If a named list, names should be target names
#' (or "all" for global exclusions) mapping to character
#' vectors of abbreviations. Converted to hub codes
#' internally. Default: NULL.
#' @param targets character vector, target name(s) to filter
#' forecasts. If NULL (default), does not filter by target.
#' @param model_ids character vector of model IDs to include.
#' If NULL (default), includes all models.
#'
#' @return tibble containing forecast summary data
#'
#' @export
summarize_ref_date_forecasts <- function(
  reference_date,
  base_hub_path,
  disease,
  population_data,
  horizons_to_include = c(0, 1, 2),
  excluded_locations = NULL,
  targets = NULL,
  model_ids = NULL
) {
  reference_date <- lubridate::as_date(reference_date)
  excluded_locations <- normalize_excluded_locations(excluded_locations)

  model_metadata <- hubData::load_model_metadata(
    base_hub_path,
    model_ids = model_ids
  )

  hub_content <- hubData::connect_hub(base_hub_path)

  current_forecasts <- hub_content |>
    dplyr::filter(
      .data$reference_date == !!reference_date,
      .data$horizon %in% !!horizons_to_include
    ) |>
    hubData::collect_hub() |>
    dplyr::filter(
      forecasttools::nullable_comparison(.data$target, "%in%", !!targets),
      forecasttools::nullable_comparison(.data$model_id, "%in%", !!model_ids)
    )

  available_targets <- unique(current_forecasts$target)
  exclusion_df <- build_exclusion_df(excluded_locations, available_targets)

  current_forecasts <- current_forecasts |>
    dplyr::anti_join(exclusion_df, by = c("target", "location"))

  if (nrow(current_forecasts) == 0) {
    model_filter_msg <- if (!is.null(model_ids)) {
      glue::glue(" and model IDs: {paste(model_ids, collapse = ', ')}")
    } else {
      ""
    }
    cli::cli_abort(
      glue::glue(
        "No forecast data found for reference date {reference_date}",
        "{model_filter_msg}"
      )
    )
  }

  forecasts_data <- forecasttools::pivot_hubverse_quantiles_wider(
    hubverse_table = current_forecasts,
    pivot_quantiles = c(
      "quantile_0.025" = 0.025,
      "quantile_0.25" = 0.25,
      "quantile_0.5" = 0.5,
      "quantile_0.75" = 0.75,
      "quantile_0.975" = 0.975
    )
  ) |>
    dplyr::mutate(
      location_name = forecasttools::us_location_recode(
        .data$location,
        "hub",
        "name"
      ),
      abbreviation = forecasttools::us_location_recode(
        .data$location,
        "hub",
        "abbr"
      )
    ) |>
    dplyr::left_join(
      population_data,
      by = "location"
    ) |>
    dplyr::mutate(
      population = as.numeric(.data$population)
    ) |>
    dplyr::mutate(
      target_data_type = get_target_data_type(.data$target)
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::starts_with("quantile_"),
        ~ dplyr::case_when(
          is_hosp_target(.data$target) ~ .x / .data$population * 100000,
          is_ed_target(.data$target) ~ NA_real_
        ),
        .names = "{.col}_per100k"
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::ends_with("_per100k"),
        ~ janitor::round_half_up(.x, 2),
        .names = "{.col}_rounded"
      ),
      dplyr::across(
        tidyselect::starts_with("quantile_") &
          !tidyselect::contains("_per100k"),
        ~ dplyr::case_when(
          is_hosp_target(.data$target) ~ round_to_place(.x),
          is_ed_target(.data$target) ~ janitor::signif_half_up(.x, digits = 2)
        ),
        .names = "{.col}_rounded"
      ),
      forecast_due_date = as.Date(!!reference_date) - 3,
      location_sort_order = ifelse(.data$location_name == "United States", 0, 1)
      # order table with national first, then alphabetically
    ) |>
    dplyr::arrange(.data$location_sort_order, .data$location_name) |>
    dplyr::left_join(
      dplyr::distinct(
        model_metadata,
        .data$model_id,
        .keep_all = TRUE
      ),
      by = "model_id"
    ) |>
    dplyr::mutate(
      reference_date = as.Date(.data$reference_date),
      target_end_date = as.Date(.data$target_end_date),
      target_end_date_formatted = format(.data$target_end_date, "%B %d, %Y"),
      reference_date_formatted = format(.data$reference_date, "%B %d, %Y"),
      forecast_due_date_formatted = format(.data$forecast_due_date, "%B %d, %Y")
    )

  return(forecasts_data)
}
