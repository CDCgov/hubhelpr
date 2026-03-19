.get_expected_quantile_oracle_output <- function(hub_path) {
  return(
    hubData::connect_target_oracle_output(
      hub_path
    ) |>
      dplyr::filter(.data$output_type == "quantile") |>
      dplyr::collect()
  )
}

.expect_oracle_tables_equivalent <- function(result, expected) {
  expected_names <- names(expected)
  checkmate::expect_names(names(result), permutation.of = expected_names)
  result_sorted <- result |>
    dplyr::mutate(output_type_id = as.character(.data$output_type_id)) |>
    dplyr::select(dplyr::all_of(expected_names)) |>
    dplyr::arrange(
      .data$target,
      .data$location,
      .data$target_end_date
    )
  expected_sorted <- expected |>
    dplyr::mutate(output_type_id = as.character(.data$output_type_id)) |>
    dplyr::arrange(
      .data$target,
      .data$location,
      .data$target_end_date
    )
  expect_equal(result_sorted, expected_sorted)
}

example_hub_paths_to_test <- purrr::pmap_vec(
  tidyr::crossing(
    version = c("v5", "v6"),
    type = c("target_dir", "target_file")
  ),
  \(version, type) {
    system.file(
      fs::path("testhubs", version, type),
      package = "hubUtils"
    )
  }
)


test_that(
  paste0(
    "oracle output can recreate canonical oracle output ",
    "for the example forecast hubs in hubUtils ",
    "for quantile targets (no guarantee for derived ",
    "targets without true values in the timeseries like cdfs)"
  ),
  {
    purrr::walk(example_hub_paths_to_test, \(hub_path) {
      result <- generate_oracle_output_table(
        hub_path,
        ts_date_col = "target_end_date"
      ) |>
        dplyr::filter(.data$output_type == "quantile")
      expected <- .get_expected_quantile_oracle_output(hub_path)
      .expect_oracle_tables_equivalent(result, expected)
    })
  }
)

test_that("write_oracle_output writes the expected table to a configurable directory", {
  output_dir <- withr::local_tempdir()
  purrr::walk(example_hub_paths_to_test, \(hub_path) {
    write_oracle_output(
      hub_path,
      output_dirpath = output_dir,
      ts_date_col = "target_end_date"
    )
    result <- forecasttools::read_tabular(fs::path(
      output_dir,
      "oracle-output",
      ext = "parquet"
    )) |>
      dplyr::filter(.data$output_type == "quantile")
    expected <- .get_expected_quantile_oracle_output(hub_path)
    .expect_oracle_tables_equivalent(result, expected)
  })
})

test_that("generate_oracle_output is an alias for write_oracle_output", {
  expect_equal(generate_oracle_output, write_oracle_output)
})
