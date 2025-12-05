#' Generate forecast data file containing all forecast hub model submissions
#'
#' This function fetches all forecast submissions from a
#' forecast hub based on the reference date. The forecast
#' data is then pivoted to create a wide format with
#' quantile levels as columns.
#'
#' @param reference_date character, the reference date for
#' the forecast in YYYY-MM-DD format (ISO-8601).
#' @param base_hub_path character, path to the forecast
#' hub directory.
#' @param hub_reports_path character, path to forecast hub
#' reports directory.
#' @param disease character, disease name ("covid" or
#' "rsv"). Used to derive target name and file prefix.
#' @param horizons_to_include integer vector, horizons to
#' include in the output. Default: c(0, 1, 2).
#' @param excluded_locations character vector of location
#' codes to exclude from the output. Default: character(0).
#' @param output_format character, output file format. One
#' of "csv", "tsv", or "parquet". Default: "csv".
#' @param targets character vector, target name(s) to filter
#' forecasts. If NULL (default), does not filter by target.
#' Can be a single target like "wk inc covid hosp" or
#' multiple targets like c("wk inc covid hosp", "wk inc
#' covid prop ed visits").
#'
#' @export
get_forecast_data <- function(
  reference_date,
  base_hub_path,
  hub_reports_path,
  disease,
  horizons_to_include = c(0, 1, 2),
  excluded_locations = character(0),
  output_format = "csv",
  targets = NULL
) {
  write_ref_date_summary_all(
    reference_date = reference_date,
    base_hub_path = base_hub_path,
    hub_reports_path = hub_reports_path,
    disease = disease,
    horizons_to_include = horizons_to_include,
    excluded_locations = excluded_locations,
    output_format = output_format,
    targets = targets
  )
}
