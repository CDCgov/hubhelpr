# First, let us create tests for the CovidHub baseline:
test_that("baseline hindcasts have the correct target date, also that all quantiles at horizon -1 have unique value and in total we have 23 quantiles", {
  base_hub_path <- fs::path(withr::local_tempdir(), "covidhub")
  fs::dir_copy(example_cfa_hub, base_hub_path)

  reference_date <- as.Date("2026-04-18")
  output_path <- fs::path(
    base_hub_path,
    "model-output",
    "CovidHub-baseline",
    "2026-04-18-CovidHub-baseline.csv"
  )

  if (fs::file_exists(output_path)) {
    fs::file_delete(output_path)
  }

  generate_hub_baseline(
    base_hub_path,
    reference_date,
    "covid",
    output_format = "csv"
  )
# after generating hub baseline file, check its existence 
  expect_true(fs::file_exists(output_path))
  forecasts <- forecasttools::read_tabular(output_path)

  hindcasts <- dplyr::filter(forecasts, horizon == -1L)
# now checking that all forecasts/hindcasts have the correct target date -- one week prior to the current reference date
  expect_true(all(as.Date(forecasts$reference_date) == reference_date))
  expect_setequal(forecasts$horizon, -1:3)
  expect_equal(
  as.Date(forecasts$target_end_date),
  reference_date + 7L * forecasts$horizon
)
  expect_true(all(
    as.Date(hindcasts$target_end_date) == reference_date - 7L
  ))
  # check all forecasts (including hindcasts) are finite
  expect_true(all(is.finite(forecasts$value)))

  checks <- hindcasts |>
    dplyr::group_by(location, target) |>
    dplyr::summarise(
      quantile_count = dplyr::n_distinct(output_type_id),
      value_count = dplyr::n_distinct(value),
      .groups = "drop"
    )

  expect_true(all(checks$quantile_count == 23L))
  expect_true(all(checks$value_count == 1L))
})

# now a final check that guarantees all predicted quantiles do not cross for each group of forecasts:

test_that("baseline quantiles are nondecreasing", {  

  base_hub_path <- fs::path(withr::local_tempdir(), "covidhub")
  fs::dir_copy(example_cfa_hub, base_hub_path)

  output_path <- fs::path(
      base_hub_path, "model-output", "CovidHub-baseline",
      "2026-04-18-CovidHub-baseline.csv"
    )
  if (fs::file_exists(output_path)) {
      fs::file_delete(output_path)
    }

  generate_hub_baseline(base_hub_path, "2026-04-18", "covid")
  forecasts <- forecasttools::read_tabular(output_path)

  expect_true(all(is.finite(forecasts$value)))

  ordered <- forecasts |>
    dplyr::mutate(quantile_level = as.numeric(output_type_id))
  expect_false(anyNA(ordered$quantile_level))

  checks <- ordered |>
    dplyr::group_by(reference_date, location, target, horizon, target_end_date) |>
    dplyr::arrange(quantile_level, .by_group = TRUE) |>
    dplyr::summarise(
      multiple_quantiles = dplyr::n_distinct(quantile_level) > 1L,
      nondecreasing = all(diff(value) >= 0),
      .groups = "drop"
    )

  expect_true(all(checks$multiple_quantiles))
  expect_true(all(checks$nondecreasing))

})