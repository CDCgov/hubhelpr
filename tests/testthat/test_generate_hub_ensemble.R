test_that("generate_hub_ensemble() errors when the ensemble already exists", {
  hub_path <- fs::path(withr::local_tempdir(), "covidhub")
  fs::dir_copy(example_cfa_hub, hub_path)

  existing_path <- fs::path(
    hub_path,
    "model-output",
    "CovidHub-ensemble",
    "2026-04-18-CovidHub-ensemble.parquet"
  )
  expect_true(fs::file_exists(existing_path))
  mtime_before <- fs::file_info(existing_path)$modification_time

  expect_error(
    generate_hub_ensemble(
      hub_path,
      "2026-04-18",
      "covid",
      output_format = "parquet"
    ),
    "already exists"
  )
  expect_identical(fs::file_info(existing_path)$modification_time, mtime_before)
})

test_that("generate_hub_ensemble() overwrites an existing ensemble on request", {
  hub_path <- fs::path(withr::local_tempdir(), "covidhub")
  fs::dir_copy(example_cfa_hub, hub_path)

  output_path <- fs::path(
    hub_path,
    "model-output",
    "CovidHub-ensemble",
    "2026-04-18-CovidHub-ensemble.parquet"
  )

  fs::file_delete(output_path)
  generate_hub_ensemble(
    hub_path,
    "2026-04-18",
    "covid",
    output_format = "parquet"
  )
  expected_ensemble <- forecasttools::read_tabular(output_path)

  # drop rows, so file left in place is distinguishable
  # from a file actually is rewritten
  forecasttools::write_tabular(
    head(expected_ensemble, 5),
    output_path
  )
  expect_identical(nrow(forecasttools::read_tabular(output_path)), 5L)

  generate_hub_ensemble(
    hub_path,
    "2026-04-18",
    "covid",
    output_format = "parquet",
    overwrite_existing = TRUE
  )

  expect_identical(
    forecasttools::read_tabular(output_path),
    expected_ensemble
  )
})

test_that("ensemble is correctly dated, and quantiles are monotone. Lastly, all locations and targets are accounted for", {
  hub_path <- fs::path(withr::local_tempdir(), "covidhub")
  fs::dir_copy(example_cfa_hub, hub_path)
  reference_date <- as.Date("2026-04-18")

  output_path <- fs::path(
    hub_path,
    "model-output",
    "CovidHub-ensemble",
    "2026-04-18-CovidHub-ensemble.parquet"
  )
  if (fs::file_exists(output_path)) {
    fs::file_delete(output_path)
  }

  generate_hub_ensemble(
    hub_path,
    reference_date,
    "covid",
    output_format = "parquet"
  )

  expect_true(fs::file_exists(output_path))
  forecasts <- forecasttools::read_tabular(output_path)


  expect_gt(nrow(forecasts), 0L)
  expect_true(all(is.finite(forecasts$value)))

  expect_true(all(as.Date(forecasts$reference_date) == reference_date))
  expect_equal(
    as.Date(forecasts$target_end_date),
    reference_date + 7L * forecasts$horizon
  )

  quantiles <- forecasts |>
    dplyr::mutate(
      quantile_level = as.numeric(as.character(output_type_id))
    )
  expect_false(anyNA(quantiles$quantile_level))

  checks <- quantiles |>
    dplyr::group_by(location, target, horizon, target_end_date) |>
    dplyr::arrange(quantile_level, .by_group = TRUE) |>
    dplyr::summarise(
      multiple_quantiles = dplyr::n_distinct(quantile_level) > 1L,
      nondecreasing = all(diff(value) >= 0),
      .groups = "drop"
    )

  expect_true(all(checks$multiple_quantiles))
  expect_true(all(checks$nondecreasing))

  expected_n_locations <- c(
    "wk inc covid hosp" = 53L,
    "wk inc covid prop ed visits" = 51L
  )

  counts <- forecasts |>
    dplyr::group_by(target) |>
    dplyr::summarise(
      n_locations = dplyr::n_distinct(location),
      n_rows = dplyr::n(),
      .groups = "drop"
    )

  expect_setequal(counts$target, names(expected_n_locations))
  expected <- c(53L, 51L)

  expect_equal(counts$n_locations, expected)
  expect_equal(counts$n_rows, expected * 5L * 23L)
  expect_equal(nrow(forecasts), sum(expected_n_locations) * 5L * 23L)
})



  



 