make_baseline_forecasts <- function(source_hub) {
  base_hub_path <- fs::path(withr::local_tempdir(), "covidhub")
  fs::dir_copy(source_hub, base_hub_path)

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

  hubhelpr::generate_hub_baseline(
    base_hub_path,
    reference_date,
    "covid",
    output_format = "csv"
  )
  forecasttools::read_tabular(output_path)
}

test_that("baseline hindcasts have the correct target date", {
  reference_date <- as.Date("2026-04-18")
  forecasts <- make_baseline_forecasts(example_cfa_hub)
  expect_true(all(as.Date(forecasts$reference_date) == reference_date))
  expect_setequal(forecasts$horizon, -1:3)
  expect_equal(
    as.Date(forecasts$target_end_date),
    reference_date + 7L * forecasts$horizon
  )
})
test_that("all forecasts are finite", {
  reference_date <- as.Date("2026-04-18")
  forecasts <- make_baseline_forecasts(example_cfa_hub)
  expect_true(all(is.finite(forecasts$value)))
})

test_that("hindcasts unique value per quantile and in total 23 quantiles", {
  reference_date <- as.Date("2026-04-18")
  forecasts <- make_baseline_forecasts(example_cfa_hub)
  hindcasts <- dplyr::filter(forecasts, horizon == -1L)

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
  reference_date <- as.Date("2026-04-18")
  forecasts <- make_baseline_forecasts(example_cfa_hub)

  expect_true(all(is.finite(forecasts$value)))

  ordered <- forecasts |>
    dplyr::mutate(quantile_level = as.numeric(output_type_id))
  expect_false(anyNA(ordered$quantile_level))

  checks <- ordered |>
    dplyr::group_by(
      reference_date,
      location,
      target,
      horizon,
      target_end_date
    ) |>
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
  expected <- unname(expected_n_locations[counts$target])

  tasks <- hubhelpr::get_hub_tasks(example_cfa_hub)
  n_horizons <- dplyr::n_distinct(tasks$horizon)

  expect_equal(counts$n_locations, expected)
  expect_equal(counts$n_rows, expected * n_horizons * 23L)
  expect_equal(nrow(forecasts), sum(expected_n_locations) * n_horizons * 23L)
})
