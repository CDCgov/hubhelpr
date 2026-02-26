#' Check hospital reporting data latency and completeness.
#'
#' This function retrieves hospital reporting data from
#' data.cdc.gov and checks for data latency and reporting
#' completeness issues. It returns a flag string
#' describing any reporting issues.
#'
#' @param reference_date Date, the reference date for the
#' forecast.
#' @param disease Character, disease name ("covid" or
#' "rsv").
#' @param included_locations Character vector of location
#' codes that are expected to report. Default
#' hubhelpr::included_locations.
#'
#' @return Character string describing reporting issues,
#' or empty string if no issues.
check_hospital_reporting_latency <- function(
  reference_date,
  disease,
  included_locations = hubhelpr::included_locations
) {
  desired_weekendingdate <- as.Date(reference_date) - lubridate::dweeks(1)

  disease_abbr <- dplyr::case_match(
    disease,
    "covid" ~ "c19",
    "rsv" ~ "rsv"
  )

  reporting_column <- glue::glue(
    "totalconf{disease_abbr}newadmperchosprepabove80pct"
  )

  included_jurisdictions <- forecasttools::us_location_recode(
    included_locations,
    "code",
    "hrd"
  )

  percent_hosp_reporting_below80 <- forecasttools::pull_data_cdc_gov_dataset(
    dataset = "nhsn_hrd_prelim",
    columns = reporting_column,
    locations = included_jurisdictions,
    start_date = as.character(desired_weekendingdate),
    end_date = as.character(desired_weekendingdate)
  ) |>
    dplyr::mutate(
      weekendingdate = as.Date(.data$weekendingdate),
      report_above_80_lgl = as.logical(
        as.numeric(.data[[reporting_column]])
      ),
      location = forecasttools::us_location_recode(
        .data$jurisdiction,
        "hrd",
        "code"
      ),
      location_name = forecasttools::us_location_recode(
        .data$jurisdiction,
        "hrd",
        "name"
      )
    )

  locations_in_data <- unique(percent_hosp_reporting_below80$location)
  missing_locations <- setdiff(included_locations, locations_in_data)

  if (length(missing_locations) > 0) {
    missing_location_names <- forecasttools::us_location_recode(
      missing_locations,
      "code",
      "name"
    )
    cli::cli_warn(
      "
      Some locations are missing from the NHSN data for the desired week.
      The reference date is {reference_date}, we expect data at least
      through {desired_weekendingdate}. However, {length(missing_locations)}
      location{?s} had no reporting data:
      {missing_location_names}.
    "
    )
  } else {
    missing_location_names <- NULL
  }

  latest_reporting_below80 <- percent_hosp_reporting_below80 |>
    dplyr::filter(
      !.data$report_above_80_lgl
    )

  flagged_location_names <- c(
    latest_reporting_below80$location_name,
    missing_location_names
  )

  any_locations_flagged <- length(flagged_location_names) > 0
  if (any_locations_flagged) {
    location_list <- cli::ansi_collapse(
      flagged_location_names,
      sep = ", ",
      last = ", and "
    )
    reporting_rate_flag <- glue::glue(
      "The following jurisdictions had <80% of hospitals reporting for ",
      "the most recent week: {location_list}. ",
      "Lower reporting rates could impact forecast validity. Percent ",
      "of hospitals reporting is calculated based on the number of active ",
      "hospitals reporting complete data to NHSN for a given reporting week."
    )
  } else {
    reporting_rate_flag <- ""
  }

  return(reporting_rate_flag)
}


#' Load the webtext template.
#'
#' Reads the webtext template file associated with the package.
#'
#' @return Character string containing the template text.
#' @noRd
load_webtext_template <- function() {
  template_path <- fs::path_package(
    "extdata",
    "webtext_template.md",
    package = "hubhelpr"
  )

  paste(readLines(template_path), collapse = "\n")
}


#' Compute the forecast change direction text.
#'
#' Compares the forecast median to the last reported value
#' and returns a human-readable direction string.
#'
#' @param forecast_value Numeric, the forecast median value.
#' @param last_reported_value Numeric, the last reported
#' observed value.
#'
#' @return Character string: "a decrease", "an increase",
#' or "a similar level".
#' @noRd
compute_change_direction <- function(
  forecast_value,
  last_reported_value
) {
  dplyr::case_when(
    forecast_value < last_reported_value ~ "a decrease",
    forecast_value > last_reported_value ~ "an increase",
    .default = "a similar level"
  )
}


#' Compute webtext template values for a single target.
#'
#' Processes data for one target and returns a named list
#' of values to fill webtext template placeholders. Keys
#' are prefixed by the target data type derived from
#' get_target_data_type() ("hosp" or "prop_ed").
#'
#' @param target Character, the target name.
#' @param disease Character, disease name.
#' @param ensemble_data Data frame of ensemble forecast data.
#' @param all_target_data Data frame of target time series
#' data.
#' @param all_forecasts_data Data frame of all forecasts data.
#' @param all_model_metadata Data frame of model metadata.
#' @param hub_name Character, hub name.
#' @param reference_date Date, the reference date.
#' @param included_locations Character vector of location
#' codes.
#'
#' @return Named list of template placeholder values with
#' keys prefixed by the target data type.
#' @noRd
compute_target_webtext_values <- function(
  target,
  disease,
  ensemble_data,
  all_target_data,
  all_forecasts_data,
  all_model_metadata,
  hub_name,
  reference_date,
  included_locations
) {
  target_type <- get_target_data_type(target)
  config <- generate_target_webtext_config(target, disease)

  target_ensemble <- ensemble_data |>
    dplyr::filter(.data$target == !!target)

  target_data <- all_target_data |>
    dplyr::filter(.data$target == !!target)

  # get contributing teams for this target
  target_contributing_models <- all_forecasts_data |>
    dplyr::filter(
      .data$target == !!target,
      .data$model != glue::glue("{hub_name}-ensemble")
    ) |>
    dplyr::pull(.data$model) |>
    unique()

  # split contributing teams by designated_model status
  contributing_metadata <- all_model_metadata |>
    dplyr::filter(.data$model_id %in% target_contributing_models)

  teams_in_ensemble <- contributing_metadata |>
    dplyr::filter(.data$designated_model) |>
    dplyr::pull(.data$team_model_text)

  teams_not_in_ensemble <- contributing_metadata |>
    dplyr::filter(!.data$designated_model) |>
    dplyr::pull(.data$team_model_text)

  median_value <- config$format_forecast(target_ensemble$quantile_0.5)
  lower_value <- config$format_forecast(target_ensemble$quantile_0.025)
  upper_value <- config$format_forecast(target_ensemble$quantile_0.975)

  last_reported_target_data <- target_data |>
    dplyr::filter(
      .data$week_ending_date == max(.data$week_ending_date),
      .data$location == "US"
    )

  last_reported_raw <- last_reported_target_data$value
  last_reported <- config$format_forecast(last_reported_raw)

  forecast_raw <- target_ensemble$quantile_0.5
  change_direction <- compute_change_direction(forecast_raw, last_reported_raw)

  target_start_date <- format(
    as.Date(min(target_data$week_ending_date)),
    "%B %d, %Y"
  )

  n_teams <- length(unique(contributing_metadata$team_name))
  n_forecasts <- length(teams_in_ensemble)

  models_included <- paste0("* ", teams_in_ensemble, collapse = "\n")
  models_not_included <- paste0("* ", teams_not_in_ensemble, collapse = "\n")

  # build named list with keys derived from target type
  values <- stats::setNames(
    list(
      median_value,
      lower_value,
      upper_value,
      change_direction,
      last_reported,
      target_start_date,
      n_teams,
      n_forecasts,
      models_included,
      models_not_included
    ),
    c(
      paste0(target_type, "_median"),
      paste0(target_type, "_lower95"),
      paste0(target_type, "_upper95"),
      paste0(target_type, "_change_direction"),
      paste0(target_type, "_last_reported"),
      paste0(target_type, "_target_start_date"),
      paste0("n_teams_", target_type),
      paste0("n_forecasts_", target_type),
      paste0("models_included_", target_type),
      paste0("models_not_included_", target_type)
    )
  )

  # add hospital reporting flag if applicable
  if (is_hosp_target(target)) {
    values[["hosp_reporting_flag_text"]] <- check_hospital_reporting_latency(
      reference_date = reference_date,
      disease = disease,
      included_locations = included_locations
    )
  }

  return(values)
}


#' Generate forecast hub webpage text block.
#'
#' Creates formatted text content for forecast hub
#' visualizations by loading a template and filling in
#' computed data values.
#'
#' @param reference_date Character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param disease Character, disease name ("covid" or
#' "rsv").
#' @param base_hub_path Character, path to the forecast
#' hub directory.
#' @param weekly_data_path Character, path to the directory
#' with weekly summary files.
#' @param targets Character vector of target names to
#' generate text for.
#' @param included_locations Character vector of location
#' codes that are expected to report. Default
#' hubhelpr::included_locations.
#' @param input_format Character, input file format for
#' reading summary data files. One of "csv", "tsv", or
#' "parquet". Default: "csv".
#'
#' @return Character string containing the formatted
#' webpage text.
#' @export
generate_webtext_block <- function(
  reference_date,
  disease,
  base_hub_path,
  weekly_data_path,
  targets,
  included_locations = hubhelpr::included_locations,
  input_format = "csv"
) {
  checkmate::assert_choice(disease, choices = c("covid", "rsv"))
  checkmate::assert_choice(input_format, choices = c("csv", "tsv", "parquet"))

  reference_date <- lubridate::as_date(reference_date)

  hub_name <- get_hub_name(disease)
  disease_display_name <- get_disease_name(disease)

  # read ensemble forecast data
  ensemble_data <- forecasttools::read_tabular(
    fs::path(
      weekly_data_path,
      glue::glue("{reference_date}_{disease}_map_data"),
      ext = input_format
    )
  ) |>
    dplyr::filter(
      .data$horizon == 1,
      .data$location_name == "United States"
    )

  # read unified target data file
  all_target_data <- forecasttools::read_tabular(
    fs::path(
      weekly_data_path,
      glue::glue("{reference_date}_{disease}_target_data"),
      ext = input_format
    )
  )

  # read forecasts data for contributing teams
  all_forecasts_data <- forecasttools::read_tabular(
    fs::path(
      weekly_data_path,
      glue::glue("{reference_date}_{disease}_forecasts_data"),
      ext = input_format
    )
  )

  # load all model metadata (including non-designated models)
  all_model_metadata <- hubData::load_model_metadata(base_hub_path) |>
    dplyr::distinct(.data$model_id, .keep_all = TRUE) |>
    dplyr::mutate(
      team_model_text = glue::glue(
        "[{team_name} (Model: {model_abbr})]({website_url})"
      )
    )

  # compute template values for each target
  target_values <- purrr::map(
    targets,
    compute_target_webtext_values,
    disease = disease,
    ensemble_data = ensemble_data,
    all_target_data = all_target_data,
    all_forecasts_data = all_forecasts_data,
    all_model_metadata = all_model_metadata,
    hub_name = hub_name,
    reference_date = reference_date,
    included_locations = included_locations
  ) |>
    purrr::list_flatten()

  # shared date values
  us_target_data <- all_target_data |>
    dplyr::filter(.data$location == "US")

  last_reported_date <- format(
    as.Date(max(us_target_data$week_ending_date)),
    "%B %d, %Y"
  )

  first_target_ensemble <- ensemble_data |>
    dplyr::filter(.data$target == !!targets[1])

  target_end_date_1wk <- first_target_ensemble$target_end_date_formatted

  target_end_date_2wk <- format(
    as.Date(first_target_ensemble$target_end_date) + lubridate::weeks(1),
    "%B %d, %Y"
  )

  # all template values
  template_values <- c(
    list(
      hub_name = hub_name,
      disease = disease_display_name,
      target_end_date_1wk = target_end_date_1wk,
      target_end_date_2wk = target_end_date_2wk,
      last_reported_date = last_reported_date
    ),
    target_values
  )

  template <- load_webtext_template()
  web_text <- glue::glue_data(template_values, template)

  return(web_text)
}


#' Generate and save text content for forecast hub
#' visualization webpage.
#'
#' Generates formatted text summary for a single disease
#' and saves it to disk. Run separately for each disease.
#'
#' @param reference_date Character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param disease Character, disease name ("covid" or "rsv").
#' @param base_hub_path Character, path to the forecast hub
#' directory.
#' @param hub_reports_path Character, path to forecast hub
#' reports directory.
#' @param targets Character vector of target names to
#' generate text for. Default NULL discovers targets
#' from hub time-series data.
#' @param included_locations Character vector of location
#' codes that are expected to report. Default
#' hubhelpr::included_locations.
#' @param input_format Character, input file format for
#' reading summary data files. One of "csv", "tsv", or
#' "parquet". Default: "csv".
#'
#' @export
write_webtext <- function(
  reference_date,
  disease,
  base_hub_path,
  hub_reports_path,
  targets = NULL,
  included_locations = hubhelpr::included_locations,
  input_format = "csv"
) {
  reference_date <- lubridate::as_date(reference_date)

  if (is.null(targets)) {
    targets <- get_unique_hub_targets(base_hub_path)
  }

  weekly_data_path <- fs::path(
    hub_reports_path,
    "weekly-summaries",
    get_hub_repo_name(disease),
    reference_date
  )

  web_text <- generate_webtext_block(
    reference_date = reference_date,
    disease = disease,
    base_hub_path = base_hub_path,
    weekly_data_path = weekly_data_path,
    targets = targets,
    included_locations = included_locations,
    input_format = input_format
  )

  output_path <- fs::path(
    weekly_data_path,
    glue::glue("{reference_date}_{disease}_webtext"),
    ext = "md"
  )

  if (fs::file_exists(output_path)) {
    cli::cli_abort("File already exists: {output_path}.")
  }

  writeLines(web_text, output_path)
  cli::cli_inform("Webtext saved as: {output_path}.")

  return(invisible())
}
