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
      "hospitals reporting complete data to NHSN for a given reporting week.\n\n"
    )
  } else {
    reporting_rate_flag <- ""
  }

  return(reporting_rate_flag)
}


#' Generate webtext for a single target.
#'
#' Internal helper function that generates the forecast
#' text block for a single target type.
#'
#' @param config List containing target-specific text and
#' formatting configuration (target_description,
#' target_short, data_source, value_unit, format_value,
#' format_forecast).
#' @param ensemble_data Data frame of ensemble forecast
#' data filtered to horizon 1 and US location for this
#' target.
#' @param target_data Data frame of target time series
#' data filtered to this target.
#' @param disease_name Character, formatted disease name
#' (e.g., "COVID-19").
#' @param hub_name Character, hub name.
#' @param reporting_rate_flag Character, reporting rate
#' flag text (only applicable for hosp target).
#'
#' @return Character string containing the target-specific
#' webtext block.
#' @noRd
generate_target_text_block <- function(
  config,
  ensemble_data,
  target_data,
  disease_name,
  hub_name,
  reporting_rate_flag = ""
) {
  # format forecast values based on target config
  forecast_value <- config$format_forecast(
    ensemble_data$quantile_0.5_count
  )
  lower_value <- config$format_forecast(
    ensemble_data$quantile_0.025_count
  )
  upper_value <- config$format_forecast(
    ensemble_data$quantile_0.975_count
  )

  # format date variables
  target_end_date_1wk_ahead <- ensemble_data$target_end_date_formatted
  target_end_date_2wk_ahead <- format(
    as.Date(ensemble_data$target_end_date) + lubridate::weeks(1),
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

  # get last reported value
  last_reported_target_data <- target_data |>
    dplyr::filter(
      .data$week_ending_date == max(.data$week_ending_date),
      .data$location == "US"
    ) |>
    dplyr::mutate(
      week_end_date_formatted = format(.data$week_ending_date, "%B %d, %Y")
    )

  last_reported <- config$format_value(last_reported_target_data$value)
  value_unit <- config$value_unit
  target_description <- glue::glue(config$target_description)
  target_short <- glue::glue(config$target_short)
  data_source <- config$data_source

  target_text <- glue::glue(
    "The {hub_name} ensemble's one-week-ahead forecast predicts that ",
    "{target_description} will be ",
    "approximately {forecast_value}{value_unit} nationally, with ",
    "{lower_value}{value_unit} to {upper_value}{value_unit} ",
    "likely reported in the week ending {target_end_date_1wk_ahead}. ",
    "This is compared to the {last_reported}{value_unit} reported for the week ",
    "ending {last_reported_target_data$week_end_date_formatted}, the most ",
    "recent week of {data_source}.\n\n",
    "The figure shows {target_description} ",
    "reported in the United States each week from ",
    "{first_target_data_date} through {last_target_data_date} and forecasted ",
    "{target_short} per week for this week and the next ",
    "2 weeks through {target_end_date_2wk_ahead}.\n\n",
    "{reporting_rate_flag}"
  )

  return(target_text)
}


#' Generate forecast hub webpage text block.
#'
#' This function creates formatted text content for
#' forecast hub visualizations. It processes forecast
#' data, target data, and team metadata to generate a
#' text description. If targets is NULL, generates text
#' for all targets present in the data. Otherwise,
#' generates text only for specified targets.
#'
#' @param reference_date Character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param disease Character, disease name ("covid" or
#' "rsv").
#' @param base_hub_path Character, path to the forecast
#' hub directory.
#' @param hub_reports_path Character, path to forecast
#' hub reports directory.
#' @param included_locations Character vector of location
#' codes that are expected to report. Default
#' hubhelpr::included_locations.
#' @param targets Character vector of target names to
#' generate text for (e.g., "wk inc covid hosp"). If NULL
#' (default), generates text for all targets present in
#' the data.
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
  hub_reports_path,
  included_locations = hubhelpr::included_locations,
  targets = NULL,
  input_format = "csv"
) {
  checkmate::assert_choice(disease, choices = c("covid", "rsv"))
  checkmate::assert_choice(input_format, choices = c("csv", "tsv", "parquet"))

  reference_date <- lubridate::as_date(reference_date)

  hub_name <- get_hub_name(disease)
  disease_name <- dplyr::case_match(
    disease,
    "covid" ~ "COVID-19",
    "rsv" ~ "RSV"
  )

  weekly_data_path <- fs::path(
    hub_reports_path,
    "weekly-summaries",
    reference_date
  )

  # read ensemble forecast data
  ensemble_data <- forecasttools::read_tabular(
    fs::path(
      weekly_data_path,
      glue::glue("{reference_date}_{disease}_map_data"),
      ext = input_format
    )
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
  contributing_teams <- forecasttools::read_tabular(
    fs::path(
      weekly_data_path,
      glue::glue("{reference_date}_{disease}_forecasts_data"),
      ext = input_format
    )
  ) |>
    dplyr::filter(.data$model != glue::glue("{hub_name}-ensemble")) |>
    dplyr::pull(.data$model) |>
    unique()

  # determine available targets from ensemble data
  available_targets <- unique(ensemble_data$target)

  # if targets is NULL, use all available targets
  if (is.null(targets)) {
    targets_to_process <- available_targets
  } else {
    # validate requested targets exist in data
    missing_targets <- setdiff(targets, available_targets)
    if (length(missing_targets) > 0) {
      cli::cli_warn(
        "Requested target{?s} not found in data: {missing_targets}."
      )
    }
    targets_to_process <- intersect(targets, available_targets)
  }

  if (length(targets_to_process) == 0) {
    cli::cli_abort("No valid targets to process.")
  }

  # mapping from target name patterns to target type keys
  get_target_type_key <- function(target_name) {
    dplyr::case_when(
      grepl("hosp", target_name) ~ "hosp",
      grepl("ed visits", target_name) ~ "ed_visits",
      .default = NA_character_
    )
  }

  # target-specific text and formatting configuration
  target_config <- list(
    hosp = list(
      target_description = "new weekly laboratory-confirmed {disease_name} hospital admissions",
      target_short = "{disease_name} hospital admissions",
      data_source = "NHSN data",
      value_unit = "",
      format_value = function(x) round(x, -2),
      format_forecast = function(x) round_to_place(x)
    ),
    ed_visits = list(
      target_description = "the proportion of emergency department visits due to {disease_name}",
      target_short = "{disease_name} ED visit proportions",
      data_source = "NSSP data",
      value_unit = "%",
      format_value = function(x) round(x * 100, 2),
      format_forecast = function(x) round(x * 100, 2)
    )
  )

  # get hospital reporting rate flag (only applies to hosp target)
  hosp_target_present <- any(grepl("hosp", targets_to_process))
  if (hosp_target_present) {
    reporting_rate_flag <- check_hospital_reporting_latency(
      reference_date = reference_date,
      disease = disease,
      included_locations = included_locations
    )
  } else {
    reporting_rate_flag <- ""
  }

  # generate text block for each target
  target_text_blocks <- purrr::map_chr(targets_to_process, function(target) {
    target_type_key <- get_target_type_key(target)

    if (is.na(target_type_key)) {
      cli::cli_warn("Unknown target type for: {target}, skipping.")
      return("")
    }

    config <- target_config[[target_type_key]]

    target_ensemble <- ensemble_data |>
      dplyr::filter(
        .data$target == !!target,
        .data$horizon == 1,
        .data$location_name == "US"
      )

    target_ts_data <- all_target_data |>
      dplyr::filter(.data$target == !!target)

    if (nrow(target_ensemble) == 0 || nrow(target_ts_data) == 0) {
      cli::cli_warn("No data found for target: {target}, skipping.")
      return("")
    }

    # only include reporting flag for hosp target
    flag_for_target <- if (target_type_key == "hosp") {
      reporting_rate_flag
    } else {
      ""
    }

    generate_target_text_block(
      config = config,
      ensemble_data = target_ensemble,
      target_data = target_ts_data,
      disease_name = disease_name,
      hub_name = hub_name,
      reporting_rate_flag = flag_for_target
    )
  })

  # combine target text blocks
  combined_target_text <- paste(
    target_text_blocks[target_text_blocks != ""],
    collapse = "\n"
  )

  # get contributing teams info
  weekly_submissions <- hubData::load_model_metadata(
    base_hub_path,
    model_ids = contributing_teams
  ) |>
    dplyr::distinct(.data$model_id, .data$designated_model, .keep_all = TRUE) |>
    dplyr::mutate(
      team_model_url = glue::glue(
        "[{team_name} (Model: {model_abbr})]({website_url})"
      )
    ) |>
    dplyr::select(
      "model_id",
      "team_abbr",
      "model_abbr",
      "team_model_url",
      "designated_model"
    )

  designated <- weekly_submissions[weekly_submissions$designated_model, ]
  not_designated <- weekly_submissions[!weekly_submissions$designated_model, ]
  weekly_num_teams <- length(unique(designated$team_abbr))
  weekly_num_models <- length(unique(designated$model_abbr))
  model_incl_in_hub_ensemble <- designated$team_model_url
  model_not_incl_in_hub_ensemble <- not_designated$team_model_url

  # get forecast due date from first available ensemble data
  forecast_due_date <- ensemble_data$forecast_due_date_formatted[1]

  # shared section: team contributions and model list
  shared_text <- glue::glue(
    "Reported and forecasted data as of ",
    "{forecast_due_date}. This week, {weekly_num_teams} modeling groups ",
    "contributed {weekly_num_models} forecasts that were eligible for inclusion ",
    "in the ensemble forecasts for at least one jurisdiction.\n\n",
    "Contributing teams and models:\n\n",
    "Models included in the {hub_name} ensemble:\n",
    "{paste(model_incl_in_hub_ensemble, collapse = '\n')}\n\n",
    "Models not included in the {hub_name} ensemble:\n",
    "{paste(model_not_incl_in_hub_ensemble, collapse = '\n')}"
  )

  web_text <- paste(combined_target_text, shared_text, sep = "\n")

  return(web_text)
}


#' Generate and save text content for forecast hub
#' visualization webpage.
#'
#' Light wrapper function that generates formatted text
#' summaries and saves them to disk.
#'
#' @param reference_date Character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param disease Character, disease name ("covid" or
#' "rsv"). Used to derive hub name, file prefix, and
#' disease display name.
#' @param base_hub_path Character, path to the forecast hub
#' directory.
#' @param hub_reports_path Character, path to forecast hub
#' reports directory.
#' @param included_locations Character vector of location
#' codes that are expected to report. Default
#' hubhelpr::included_locations.
#' @param targets Character vector of target names to
#' generate text for (e.g., "wk inc covid hosp"). If NULL
#' (default), generates text for all targets present in
#' the data.
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
  included_locations = hubhelpr::included_locations,
  targets = NULL,
  input_format = "csv"
) {
  reference_date <- lubridate::as_date(reference_date)

  web_text <- generate_webtext_block(
    reference_date = reference_date,
    disease = disease,
    base_hub_path = base_hub_path,
    hub_reports_path = hub_reports_path,
    included_locations = included_locations,
    targets = targets,
    input_format = input_format
  )

  weekly_data_path <- fs::path(
    hub_reports_path,
    "weekly-summaries",
    reference_date
  )

  output_filepath <- fs::path(
    weekly_data_path,
    glue::glue("{reference_date}_webtext"),
    ext = "md"
  )

  fs::dir_create(weekly_data_path)

  if (!fs::file_exists(output_filepath)) {
    writeLines(web_text, output_filepath)
    cli::cli_inform("Webtext saved as: {output_filepath}.")
  } else {
    cli::cli_abort("File already exists: {output_filepath}.")
  }

  return(invisible())
}
