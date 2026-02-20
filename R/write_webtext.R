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


#' Compute webtext components for a single target.
#'
#' Internal helper that processes one target, filtering
#' data, gathering contributing teams, and computing the
#' text components needed by generate_webtext_block() to
#' assemble the final document.
#'
#' @param target Character, the target name.
#' @param disease Character, disease name.
#' @param ensemble_data Data frame of ensemble forecast data.
#' @param all_target_data Data frame of target time series data.
#' @param all_forecasts_data Data frame of all forecasts data.
#' @param all_model_metadata Data frame of model metadata.
#' @param hub_name Character, hub name.
#' @param reference_date Date, the reference date.
#' @param included_locations Character vector of location codes.
#'
#' @return Named list with components: forecast_paragraph,
#' forecast_due_date, overview_fragment,
#' figure_description, forecast_horizon_text,
#' model_list_text, reporting_flag.
#'
generate_target_text_block <- function(
  target,
  disease,
  ensemble_data,
  all_target_data,
  all_forecasts_data,
  all_model_metadata,
  hub_name,
  reference_date,
  included_locations,
  also_predicts = FALSE
) {
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

  # pre-format overview fragment
  n_modeling_groups <- length(unique(contributing_metadata$team_name))
  n_forecasts <- length(target_contributing_models)
  overview_fragment <- glue::glue(
    "{n_modeling_groups} modeling groups contributed ",
    "{n_forecasts} forecasts of {tolower(config$section_header)}"
  )

  # pre-format model lists
  model_list_text <- glue::glue(
    "Models included in {hub_name} ensemble ({config$section_header}):",
    "{paste0('* ', teams_in_ensemble, collapse = '\n')}",
    "",
    "Models not included in {hub_name} ensemble ({config$section_header}):",
    "{paste0('* ', teams_not_in_ensemble, collapse = '\n')}",
    .sep = "\n"
  )

  # format forecast values based on target config
  forecast_value <- config$format_forecast(target_ensemble$quantile_0.5)
  lower_value <- config$format_forecast(target_ensemble$quantile_0.025)
  upper_value <- config$format_forecast(target_ensemble$quantile_0.975)

  # format date variables
  target_end_date_1wk_ahead <- target_ensemble$target_end_date_formatted
  target_end_date_2wk_ahead <- format(
    as.Date(target_ensemble$target_end_date) + lubridate::weeks(1),
    "%B %d, %Y"
  )
  first_target_data_date <- format(
    as.Date(min(target_data$week_ending_date)),
    "%B %d, %Y"
  )
  last_target_data_date <- format(
    as.Date(max(target_data$week_ending_date)),
    "%B %d, %Y"
  )
  forecast_due_date <- target_ensemble$forecast_due_date_formatted

  # get last reported value
  last_reported_target_data <- target_data |>
    dplyr::filter(
      .data$week_ending_date == max(.data$week_ending_date),
      .data$location == "US"
    ) |>
    dplyr::mutate(
      week_end_date_formatted = format(.data$week_ending_date, "%B %d, %Y")
    )

  last_reported <- config$format_forecast(last_reported_target_data$value)
  value_unit <- config$value_unit
  target_description <- config$target_description
  target_short <- config$target_short
  data_source <- config$data_source

  direction <- dplyr::case_when(
    forecast_value < last_reported ~ "a decrease",
    forecast_value > last_reported ~ "an increase",
    TRUE ~ "similar to the value"
  )

  predicts_phrase <- if (also_predicts) {
    "ensemble forecasting also predicts"
  } else {
    "ensemble predicts"
  }

  forecast_paragraph <- glue::glue(
    "The {hub_name} {predicts_phrase} that for the week ending ",
    "{target_end_date_1wk_ahead}, {target_description} in the United ",
    "States will be {forecast_value}{value_unit} ",
    "(95% prediction interval: {lower_value}{value_unit} to ",
    "{upper_value}{value_unit}). This is compared to the ",
    "{last_reported}{value_unit} reported for the week ending ",
    "{last_reported_target_data$week_end_date_formatted}, the most ",
    "recent week of {data_source}, {direction} from the prior week."
  )

  figure_description <- glue::glue(
    "{target_description} reported in the United States each week from ",
    "{first_target_data_date} through {last_target_data_date}"
  )

  forecast_horizon_text <- glue::glue(
    "forecasted {target_short} per week for this week and the next ",
    "2 weeks through {target_end_date_2wk_ahead}"
  )

  reporting_flag <- ""
  if (is_hosp_target(target)) {
    reporting_flag <- check_hospital_reporting_latency(
      reference_date = reference_date,
      disease = disease,
      included_locations = included_locations
    )
  }

  return(list(
    forecast_paragraph = forecast_paragraph,
    forecast_due_date = forecast_due_date,
    overview_fragment = overview_fragment,
    figure_description = figure_description,
    forecast_horizon_text = forecast_horizon_text,
    model_list_text = model_list_text,
    reporting_flag = reporting_flag
  ))
}


#' Generate forecast hub webpage text block.
#'
#' Creates formatted text content for forecast hub
#' visualizations. Processes forecast data, target data,
#' and team metadata to generate a combined text
#' description across all specified targets.
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
#' generate text for (e.g., "wk inc covid hosp").
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

  # identify ed and hosp targets
  ed_target <- targets[purrr::map_lgl(targets, is_ed_target)]
  hosp_target <- targets[purrr::map_lgl(targets, is_hosp_target)]

  # get components for each target
  ed <- generate_target_text_block(
    target = ed_target,
    disease = disease,
    ensemble_data = ensemble_data,
    all_target_data = all_target_data,
    all_forecasts_data = all_forecasts_data,
    all_model_metadata = all_model_metadata,
    hub_name = hub_name,
    reference_date = reference_date,
    included_locations = included_locations
  )

  hosp <- generate_target_text_block(
    target = hosp_target,
    disease = disease,
    ensemble_data = ensemble_data,
    all_target_data = all_target_data,
    all_forecasts_data = all_forecasts_data,
    all_model_metadata = all_model_metadata,
    hub_name = hub_name,
    reference_date = reference_date,
    included_locations = included_locations,
    also_predicts = TRUE
  )

  # build webtext from template
  web_text <- glue::glue(
    "{ed$forecast_paragraph}",
    "",
    "{hosp$forecast_paragraph}",
    "",
    "Overview: Reported and forecasted data as of {ed$forecast_due_date}. ",
    "{ed$overview_fragment} and {hosp$overview_fragment}.",
    "",
    "What does the figure show?: The figure shows ",
    "{ed$figure_description} and {hosp$figure_description}, ",
    "{hosp$forecast_horizon_text}.",
    "",
    "{hosp$reporting_flag}",
    "",
    "{ed$model_list_text}",
    "",
    "{hosp$model_list_text}",
    .sep = "\n"
  )

  # clean up empty sections (due to blank reporting flag)
  web_text <- stringr::str_replace_all(web_text, "\n{3,}", "\n\n")

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
#' generate text for (e.g., "wk inc covid hosp"). If
#' NULL (default), targets are discovered from the hub
#' time-series data via [get_unique_hub_targets()].
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
