#' Generate text content for forecast hub visualization webpage.
#'
#' This function generates formatted text summaries for the
#' forecast hub visualization webpage. It processes
#' ensemble forecast data, target hospital admissions data,
#' and contributing team information to create a text
#' description of the current week's hospitalization
#' forecasts.
#'
#' @param reference_date character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param disease character, disease name ("covid" or
#' "rsv"). Used to derive hub name, file prefix, and
#' disease display name.
#' @param base_hub_path character, path to the forecast hub
#' directory. Default: "."
#' @param hub_reports_path character, path to forecast hub
#' reports directory. Default: "../covidhub-reports"
#' @param excluded_territories character vector of location
#' codes to exclude from reporting calculations. Default:
#' character(0).
#'
#' @export
get_webtext <- function(
  reference_date,
  disease,
  base_hub_path = ".",
  hub_reports_path = "../covidhub-reports",
  excluded_territories = character(0)
) {
  checkmate::assert_choice(disease, choices = c("covid", "rsv"))
  checkmate::assert_character(excluded_territories)

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

  # could possibly use write_ref_date_summary_ensemble() or summarize_ref_date_forecasts()
  ensemble_us_1wk_ahead <- forecasttools::read_tabular(
    fs::path(
      weekly_data_path,
      glue::glue("{reference_date}_{disease}_map_data"),
      ext = "csv"
    )
  ) |>
    dplyr::filter(horizon == 1, location_name == "US")

  target_data <- forecasttools::read_tabular(
    fs::path(
      weekly_data_path,
      glue::glue(
        "{reference_date}_{disease}_target_hospital_admissions_data"
      ),
      ext = "csv"
    )
  )

  contributing_teams <- forecasttools::read_tabular(
    fs::path(
      weekly_data_path,
      glue::glue("{reference_date}_{disease}_forecasts_data"),
      ext = "csv"
    )
  ) |>
    dplyr::filter(model != glue::glue("{hub_name}-ensemble")) |>
    dplyr::pull(model) |>
    unique()

  wkly_submissions <- hubData::load_model_metadata(
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
      model_id,
      team_abbr,
      model_abbr,
      team_model_url,
      designated_model
    )

  desired_weekendingdate <- as.Date(reference_date) - lubridate::dweeks(1)

  percent_hosp_reporting_below80 <- forecasttools::pull_data_cdc_gov_dataset(
    dataset = "mpgq-jmmr",
    columns = c("totalconfc19newadmperchosprepabove80pct"),
    start_date = "2024-11-09"
  ) |>
    dplyr::mutate(
      weekendingdate = as.Date(.data$weekendingdate),
      report_above_80_lgl = as.logical(
        as.numeric(.data$totalconfc19newadmperchosprepabove80pct)
      ),
      jurisdiction = dplyr::case_match(
        .data$jurisdiction,
        "USA" ~ "US",
        .default = .data$jurisdiction
      ),
      location = forecasttools::us_location_recode(
        .data$jurisdiction,
        "abbr",
        "code"
      ),
      location_name = forecasttools::us_location_recode(
        .data$jurisdiction,
        "abbr",
        "name"
      )
    ) |>
    dplyr::filter(!(.data$location %in% !!excluded_territories)) |>
    dplyr::group_by(.data$jurisdiction) |>
    dplyr::mutate(max_weekendingdate = max(.data$weekendingdate)) |>
    dplyr::ungroup()

  jurisdiction_w_latency <- percent_hosp_reporting_below80 |>
    dplyr::filter(.data$max_weekendingdate < !!desired_weekendingdate)

  if (nrow(jurisdiction_w_latency) > 0) {
    cli::cli_warn(
      "
      Some locations have missing reported data for the most recent week.
      The reference date is {reference_date}, we expect data at least
      through {desired_weekendingdate}. However, {nrow(jurisdiction_w_latency)}
      location{?s} did not have reporting through that date:
      {jurisdiction_w_latency$location_name}.
    "
    )
  }

  latest_reporting_below80 <- percent_hosp_reporting_below80 |>
    dplyr::filter(
      .data$weekendingdate == max(.data$weekendingdate),
      !.data$report_above_80_lgl
    )

  reporting_rate_flag <- if (
    length(latest_reporting_below80$location_name) > 0
  ) {
    location_list <- if (length(latest_reporting_below80$location_name) < 3) {
      glue::glue_collapse(latest_reporting_below80$location_name, sep = " and ")
    } else {
      glue::glue_collapse(
        latest_reporting_below80$location_name,
        sep = ", ",
        last = ", and "
      )
    }

    glue::glue(
      "The following jurisdictions had <80% of hospitals reporting for ",
      "the most recent week: {location_list}. ",
      "Lower reporting rates could impact forecast validity. Percent ",
      "of hospitals reporting is calculated based on the number of active ",
      "hospitals reporting complete data to NHSN for a given reporting week.\n\n"
    )
  } else {
    ""
  }

  round_to_place <- function(value) {
    if (value >= 1000) {
      rounded_val <- round(value, -2)
    } else if (value >= 10) {
      rounded_val <- round(value, -1)
    } else {
      rounded_val <- round(value, 0)
    }
    return(rounded_val)
  }

  # generate variables used in the web text

  median_forecast_1wk_ahead <- round_to_place(
    ensemble_us_1wk_ahead$quantile_0.5_count
  )
  lower_95ci_forecast_1wk_ahead <- round_to_place(
    ensemble_us_1wk_ahead$quantile_0.025_count
  )
  upper_95ci_forecast_1wk_ahead <- round_to_place(
    ensemble_us_1wk_ahead$quantile_0.975_count
  )

  designated <- wkly_submissions[wkly_submissions$designated_model, ]
  not_designated <- wkly_submissions[!wkly_submissions$designated_model, ]
  weekly_num_teams <- length(unique(designated$team_abbr))
  weekly_num_models <- length(unique(designated$model_abbr))
  model_incl_in_hub_ensemble <- designated$team_model_url
  model_not_incl_in_hub_ensemble <- not_designated$team_model_url

  first_target_data_date <- format(
    as.Date(min(target_data$week_ending_date)),
    "%B %d, %Y"
  )
  last_target_data_date <- format(
    as.Date(max(target_data$week_ending_date)),
    "%B %d, %Y"
  )
  forecast_due_date <- ensemble_us_1wk_ahead$forecast_due_date_formatted
  target_end_date_1wk_ahead <- ensemble_us_1wk_ahead$target_end_date_formatted
  target_end_date_2wk_ahead <- format(
    ensemble_us_1wk_ahead$target_end_date + lubridate::weeks(1),
    "%B %d, %Y"
  )

  last_reported_target_data <- target_data |>
    dplyr::filter(
      week_ending_date == max(week_ending_date),
      location == "US"
    ) |>
    dplyr::mutate(
      week_end_date_formatted = format(week_ending_date, "%B %d, %Y")
    )

  last_reported_admissions <- round(last_reported_target_data$value, -2)

  web_text <- glue::glue(
    "The {hub_name} ensemble's one-week-ahead forecast predicts that the number ",
    "of new weekly laboratory-confirmed {disease_name} hospital admissions will be ",
    "approximately {median_forecast_1wk_ahead} nationally, with ",
    "{lower_95ci_forecast_1wk_ahead} to {upper_95ci_forecast_1wk_ahead} ",
    "laboratory confirmed {disease_name} hospital admissions likely reported in the ",
    "week ending {target_end_date_1wk_ahead}. This is compared to the ",
    "{last_reported_admissions} admissions reported for the week ",
    "ending {last_reported_target_data$week_end_date_formatted}, the most ",
    "recent week of reporting from U.S. hospitals.\n\n",
    "Reported and forecasted new {disease_name} hospital admissions as of ",
    "{forecast_due_date}. This week, {weekly_num_teams} modeling groups ",
    "contributed {weekly_num_models} forecasts that were eligible for inclusion ",
    "in the ensemble forecasts for at least one jurisdiction.\n\n",
    "The figure shows the number of new laboratory-confirmed {disease_name} hospital ",
    "admissions reported in the United States each week from ",
    "{first_target_data_date} through {last_target_data_date} and forecasted ",
    "new {disease_name} hospital admissions per week for this week and the next ",
    "2 weeks through {target_end_date_2wk_ahead}.\n\n",
    "{reporting_rate_flag}\n",
    "Contributing teams and models:\n\n",
    "Models included in the {hub_name} ensemble:\n",
    "{paste(model_incl_in_hub_ensemble, collapse = '\n')}\n\n",
    "Models not included in the {hub_name} ensemble:\n",
    "{paste(model_not_incl_in_hub_ensemble, collapse = '\n')}"
  )

  output_filepath <- fs::path(
    weekly_data_path,
    glue::glue("{reference_date}_webtext"),
    ext = "md"
  )

  fs::dir_create(weekly_data_path)

  if (!fs::file_exists(output_filepath)) {
    writeLines(web_text, output_filepath)
    cli::cli_inform("Webtext saved as: {output_filepath}")
  } else {
    cli::cli_abort("File already exists: {output_filepath}")
  }

  return(invisible())
}
