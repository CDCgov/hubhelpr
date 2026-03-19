test_that(
  paste0(
    "oracle output can recreate canonical oracle output ",
    "for the example forecast hubs in hubUtils ",
    "for quantile targets (no guarantee for derived ",
    "targets without true values in the timeseries like cdfs)"
  ),
  {
    for (version in c("v5", "v6")) {
      hub_path <- system.file(
        as.character(glue::glue("testhubs/{version}/target_dir")),
        package = "hubUtils"
      )
      result <- generate_oracle_output_table(
        hub_path,
        ts_date_col = "target_end_date"
      ) |>
        dplyr::filter(.data$output_type == "quantile") |>
        dplyr::mutate(output_type_id = as.character(.data$output_type_id))
      expected <- hubData::connect_target_oracle_output(
        hub_path
      ) |>
        dplyr::filter(.data$output_type == "quantile") |>
        dplyr::collect()
      expected_names <- names(expected)
      checkmate::expect_names(names(result), permutation.of = expected_names)
      result_sorted <- result |>
        dplyr::select(dplyr::all_of(expected_names)) |>
        dplyr::arrange(
          .data$target,
          .data$location,
          .data$target_end_date
        )
      expected_sorted <- expected |>
        dplyr::arrange(
          .data$target,
          .data$location,
          .data$target_end_date
        )
      expect_equal(result_sorted, expected_sorted)
    }
  }
)
