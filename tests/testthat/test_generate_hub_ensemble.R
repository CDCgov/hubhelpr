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
  existing_ensemble <- forecasttools::read_tabular(output_path)

  expect_no_error(
    generate_hub_ensemble(
      hub_path,
      "2026-04-18",
      "covid",
      output_format = "parquet",
      overwrite_existing = TRUE
    )
  )
  expect_identical(
    colnames(forecasttools::read_tabular(output_path)),
    colnames(existing_ensemble)
  )
})
