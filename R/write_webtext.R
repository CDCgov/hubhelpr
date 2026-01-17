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


#' Generate webtext for a single target.
#'
#' Internal helper function that generates the forecast
#' text block for a single target type.
#'
#' @param config List containing target-specific text and
#' formatting configuration (section_header, target_description,
#' target_short, data_source, value_unit, format_value,
#' format_forecast).
#' @param ensemble_data Data frame of ensemble forecast
#' data filtered to horizon 1 and US location for this
#' target.
#' @param target_data Data frame of target time series
#' data filtered to this target.
#' @param hub_name Character, hub name.
#' @param contributing_teams_text Character, formatted text
#' listing contributing teams and models for this target.
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
  hub_name,
  contributing_teams_text,
  reporting_rate_flag = ""
) {
  # format forecast values based on target config
  forecast_value <- config$format_forecast(
    ensemble_data$quantile_0.5
  )
  lower_value <- config$format_forecast(
    ensemble_data$quantile_0.025
  )
  upper_value <- config$format_forecast(
    ensemble_data$quantile_0.975
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

  forecast_due_date <- ensemble_data$forecast_due_date_formatted

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
  target_description <- config$target_description
  target_short <- config$target_short
  data_source <- config$data_source
  section_header <- config$section_header

  # build points by bullet
  bullets <- c(
    glue::glue(
      "The {hub_name} ensemble's one-week-ahead forecast predicts that ",
      "{target_description} will be ",
      "approximately {forecast_value}{value_unit} nationally, with ",
      "{lower_value}{value_unit} to {upper_value}{value_unit} ",
      "likely reported in the week ending {target_end_date_1wk_ahead}."
    ),
    glue::glue(
      "This is compared to the {last_reported}{value_unit} reported for the week ",
      "ending {last_reported_target_data$week_end_date_formatted}, the most ",
      "recent week of {data_source}."
    ),
    glue::glue(
      "Reported and forecasted data as of {forecast_due_date}."
    ),
    glue::glue(
      "The figure shows {target_description} ",
      "reported in the United States each week from ",
      "{first_target_data_date} through {last_target_data_date} and forecasted ",
      "{target_short} per week for this week and the next ",
      "2 weeks through {target_end_date_2wk_ahead}."
    )
  )

  # add reporting rate flag if present (hosp only)
  if (nchar(reporting_rate_flag) > 0) {
    bullets <- c(bullets, reporting_rate_flag)
  }

  bullets <- c(bullets, contributing_teams_text)

  # format as bullet list with section header
  bullet_text <- paste0("* ", bullets, collapse = "\n")
  target_text <- glue::glue("## {section_header}\n\n{bullet_text}\n")

  return(target_text)
}


#' Generate forecast hub webpage text block.
#'
#' This function creates formatted text content for
#' forecast hub visualizations. It processes forecast
#' data, target data, and team metadata to generate a
#' text description for the specified targets.
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
  available_targets <- unique(ensemble_data$target)

  # validate requested targets exist in data
  missing_targets <- setdiff(targets, available_targets)
  if (length(missing_targets) > 0) {
    cli::cli_warn(
      "Requested target{?s} not found in data: {missing_targets}."
    )
  }
  targets <- intersect(targets, available_targets)

  if (length(targets) == 0) {
    cli::cli_abort("No valid targets to process.")
  }

  # load all model metadata
  all_model_metadata <- hubData::load_model_metadata(base_hub_path) |>
    dplyr::distinct(.data$model_id, .keep_all = TRUE) |>
    dplyr::filter(.data$designated_model) |>
    dplyr::mutate(
      team_model_text = dplyr::if_else(
        !is.na(.data$website_url) & nchar(.data$website_url) > 0,
        glue::glue("[{team_name} (Model: {model_abbr})]({website_url})"),
        glue::glue("{team_name} (Model: {model_abbr})")
      )
    )

  # generate text block for each target
  target_text_blocks <- purrr::map_chr(targets, function(target) {
    config <- generate_target_webtext_config(target, disease)

    if (is.null(config)) {
      return("")
    }

    target_ensemble <- ensemble_data |>
      dplyr::filter(.data$target == !!target)

    target_ts_data <- all_target_data |>
      dplyr::filter(.data$target == !!target)

    if (nrow(target_ensemble) == 0 || nrow(target_ts_data) == 0) {
      cli::cli_warn("No data found for target: {target}, skipping.")
      return("")
    }

    # get contributing teams for this target
    target_contributing_models <- all_forecasts_data |>
      dplyr::filter(
        .data$target == !!target,
        .data$model != glue::glue("{hub_name}-ensemble")
      ) |>
      dplyr::pull(.data$model) |>
      unique()

    contributing_teams_text <- all_model_metadata |>
      dplyr::filter(.data$model_id %in% target_contributing_models) |>
      dplyr::pull(.data$team_model_text) |>
      paste(collapse = ", ") |>
      (\(x) glue::glue("Contributing teams and models: {x}"))()

    # get hospital reporting flag for hosp targets only
    reporting_rate_flag <- if (is_hosp_target(target)) {
      tryCatch(
        check_hospital_reporting_latency(
          reference_date = reference_date,
          disease = disease,
          included_locations = included_locations
        ),
        error = function(e) {
          cli::cli_warn(
            "Could not retrieve hospital reporting data: {e$message}"
          )
          return("")
        }
      )
    } else {
      ""
    }

    generate_target_text_block(
      config = config,
      ensemble_data = target_ensemble,
      target_data = target_ts_data,
      hub_name = hub_name,
      contributing_teams_text = contributing_teams_text,
      reporting_rate_flag = reporting_rate_flag
    )
  })

  disease_display_name <- get_disease_name(disease)

  # combine target text blocks with H1 disease header
  target_sections <- paste(
    target_text_blocks[target_text_blocks != ""],
    collapse = "\n\n"
  )
  web_text <- glue::glue("# {disease_display_name}\n\n{target_sections}")

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
#' generate text for (e.g., "wk inc covid hosp").
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
  targets,
  included_locations = hubhelpr::included_locations,
  input_format = "csv"
) {
  reference_date <- lubridate::as_date(reference_date)
  if (is.na(reference_date)) {
    cli::cli_abort("Invalid reference_date format.")
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
