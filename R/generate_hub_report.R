#' Generate a full report for a disease and reference date
#' with sensible, consistent configuration.
#'
#' Wrapper function for [write_ref_date_summary_ens()],
#' [write_ref_date_summary_all()], [write_viz_target_data()],
#' and [write_webtext()].
#'
#' @param reference_date character, the reference date
#' for the forecast in YYYY-MM-DD format (ISO-8601).
#' @param disease character, disease name ("covid" or
#' "rsv").
#' @param base_hub_path character, path to the forecast
#' hub directory.
#' @param hub_reports_path character, path to forecast
#' hub reports directory.
#' @param horizons integer vector, forecast horizons
#' to include in the output. Default: c(0, 1, 2).
#' @param population_data data frame with columns
#' "location" and "population". Used to compute per-population
#' metrics such as rates per 100k from rate counts.
#' Default: [hubhelpr::population_data].
#' @param excluded_locations NULL, character vector, or
#' named list of US state/territory abbreviations to
#' exclude. If a character vector, locations are
#' excluded across all targets. If a named list, names
#' should be target names (or "all" for global
#' exclusions) mapping to character vectors of
#' abbreviations. Default: NULL (no exclusions).
#' @param output_format character, output file format.
#' One of "csv", "tsv", or "parquet". Default: "csv".
#' @param targets character vector, target name(s) to
#' filter forecasts for reporting. If NULL (default),
#' does not filter out targets.
#' @param n_models_for_ens_reporting integer, minimum
#' number of designated model submissions required to
#' include an ensemble forecast in the report. Default:
#' NULL, which uses the minimum designated for
#' `reference_date`, so that regenerating an older report
#' applies the rule it was published under rather than
#' today's. See [get_reference_date_ens_minimum()].
#' @param overwrite_existing logical. If TRUE, overwrite
#' existing files. Default: FALSE.
#' @param observed_data_use_hub Logical, whether to read data from
#' the hub's time-series file (TRUE) or pull fresh data
#' from raw sources (FALSE) when creating observed data files.
#' Default: FALSE.
#' @param observed_data_as_of As of date for the observed
#' data shown in the report. Either an object coercible by
#' [as.Date()] or "latest" to use the most recent
#' available vintage. Default "latest". Used only when
#' `observed_data_use_hub = TRUE`.
#' @param observed_data_start_date Date, earliest date to include in observed
#' data shown in the report. Default: NULL (no filtering).
#' Used only when `observed_data_use_hub = FALSE`.
#' @param observed_data_start_date Date, latest date to include in observed
#' data shown in the report. Default: NULL (no filtering).
#' Used only when `observed_data_use_hub = FALSE`.
#' @param verbose Report progress to the terminal? Default `TRUE`.
#' @return NULL, invsibly
#' @export
generate_hub_report <- function(
  reference_date,
  disease,
  hub_reports_path,
  base_hub_path = NULL,
  horizons = c(0, 1, 2),
  population_data = hubhelpr::population_data,
  excluded_locations = NULL,
  targets = NULL,
  n_models_for_ens_reporting = NULL,
  output_format = "csv",
  overwrite_existing = FALSE,
  observed_data_use_hub = TRUE,
  observed_data_as_of = "latest",
  observed_data_start_date = NULL,
  observed_data_end_date = NULL,
  verbose = TRUE
) {
  base_hub_path <- base_hub_path %||% hub_cloud_path(disease)
  n_models_for_ens_reporting <- n_models_for_ens_reporting %||%
    get_reference_date_ens_minimum(
      hub_reports_path,
      disease,
      reference_date
    )
  if (verbose) {
    cli::cli_inform(c(
      "Starting hub report generation for disease ",
      "{disease} and reference date {reference_date} ",
      "using hub at {base_hub_path} and writing ",
      "to {hub_reports_path}..."
    ))
  }

  if (verbose) {
    cli::cli_inform("Writing ensemble summary file...")
  }

  write_ref_date_summary_ens(
    reference_date = reference_date,
    base_hub_path = base_hub_path,
    hub_reports_path = hub_reports_path,
    disease = disease,
    horizons_to_include = horizons,
    population_data = population_data,
    output_format = output_format,
    targets = targets,
    excluded_locations = excluded_locations,
    n_models_for_ens_reporting = n_models_for_ens_reporting,
    overwrite_existing = overwrite_existing
  )

  if (verbose) {
    cli::cli_inform("Writing all model summary file...")
  }
  write_ref_date_summary_all(
    reference_date = reference_date,
    base_hub_path = base_hub_path,
    hub_reports_path = hub_reports_path,
    disease = disease,
    horizons_to_include = horizons,
    population_data = population_data,
    excluded_locations = excluded_locations,
    output_format = output_format,
    targets = targets,
    n_models_for_ens_reporting = n_models_for_ens_reporting,
    overwrite_existing = overwrite_existing
  )

  if (verbose) {
    cli::cli_inform("Writing observed data file...")
  }
  write_viz_target_data(
    reference_date = reference_date,
    base_hub_path = base_hub_path,
    hub_reports_path = hub_reports_path,
    disease = disease,
    use_hub_data = observed_data_use_hub,
    as_of = observed_data_as_of,
    start_date = observed_data_start_date,
    end_date = observed_data_end_date,
    excluded_locations = excluded_locations,
    output_format = output_format,
    overwrite_existing = overwrite_existing
  )
  if (verbose) {
    cli::cli_inform("Writing webtext...")
  }
  write_webtext(
    reference_date = reference_date,
    disease = disease,
    base_hub_path = base_hub_path,
    hub_reports_path = hub_reports_path,
    targets = targets,
    excluded_locations = excluded_locations,
    input_format = output_format, # reads the tables written above
    overwrite_existing = overwrite_existing
  )

  invisible()
}
