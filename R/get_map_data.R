#' Generate map data file containing ensemble forecast data
#'
#' This function loads the latest ensemble forecast data
#' from the forecast hub and processes it into the required
#' format. The resulting file contains forecast values for
#' all regions (including US, DC, and Puerto Rico), for
#' various forecast horizons, and quantiles (0.025, 0.5,
#' and 0.975).
#'
#' @param reference_date character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param base_hub_path character, path to the forecast
#' hub directory.
#' @param hub_reports_path character, path to forecast hub
#' reports directory.
#' @param disease character, disease name ("covid" or
#' "rsv"). Used to derive hub name and file prefix.
#' @param horizons_to_include integer vector, horizons to
#' include in the output. Default: c(0, 1, 2).
#' @param population_data data frame with columns
#' "location" and "population".
#' @param excluded_locations character vector of location
#' codes to exclude from the output. Default: character(0).
#' @param output_format character, output file format. One
#' of "csv", "tsv", or "parquet". Default: "csv".
#'
#' @export
get_map_data <- function(
  reference_date,
  base_hub_path,
  hub_reports_path,
  disease,
  horizons_to_include = c(0, 1, 2),
  population_data,
  excluded_locations = character(0),
  output_format = "csv"
) {
  write_ref_date_summary_ensemble(
    reference_date = reference_date,
    base_hub_path = base_hub_path,
    hub_reports_path = hub_reports_path,
    disease = disease,
    horizons_to_include = horizons_to_include,
    population_data = population_data,
    excluded_locations = excluded_locations,
    output_format = output_format
  )
}
