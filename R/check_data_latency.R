#' Checks latency of input data.
#'
#' @param data Data frame containing target data.
#' @param target_label Character. Human-readable label for the target.
#' @param expected_max_time_value Date. The most recent date for which
#' observations are expected.
#' @param location_col_name Character. Name of the location column.
#' Default is "geo_value".
#' @param date_col_name Character. Name of the date column.
#' Default is "time_value".
#' @param overlatent_err_thresh Numeric. Proportion threshold for raising error
#' vs warning. Default 0.20.
#'
#' @return Invisible NULL. Raises warnings or errors
#' based on the proportion of locations with excess latency.
#' @export
check_data_latency <- function(
  data,
  target_label,
  expected_max_time_value,
  location_col_name = "geo_value",
  date_col_name = "time_value",
  overlatent_err_thresh = 0.20
) {
  latency_tbl <- data |>
    dplyr::group_by(.data[[location_col_name]]) |>
    dplyr::summarise(
      latest_date = max(.data[[date_col_name]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      excess_latency = pmax(
        as.integer(expected_max_time_value - .data$latest_date) %/% 7L,
        0L
      ),
      has_excess_latency = .data$excess_latency > 0L
    )

  prop_locs_overlatent <- mean(latency_tbl$has_excess_latency)

  if (prop_locs_overlatent > overlatent_err_thresh) {
    cli::cli_abort(
      c(
        "{target_label}: More than {100 * overlatent_err_thresh}% of locations have excess latency.",
        "!" = "Expected observations through: {expected_max_time_value}",
        "!" = "{nrow(latency_tbl |> dplyr::filter(.data$has_excess_latency))} location{?s} have excess latency"
      )
    )
  } else if (prop_locs_overlatent > 0) {
    cli::cli_warn(
      c(
        "{target_label}: Some locations have excess latency.",
        "!" = "Expected observations through: {expected_max_time_value}",
        "!" = "{nrow(latency_tbl |> dplyr::filter(.data$has_excess_latency))} location{?s} have excess latency"
      )
    )
  }
  return(invisible())
}
