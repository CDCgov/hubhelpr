test_that("summarize_ref_date_forecasts includes 0.10 and 0.90 quantiles", {
  summary_data <- summarize_ref_date_forecasts(
    reference_date = "2026-04-11",
    base_hub_path = example_cfa_hub,
    disease = "covid"
  )

  checkmate::expect_names(
    names(summary_data),
    must.include = c(
      "quantile_0.10",
      "quantile_0.90",
      "quantile_0.10_rounded",
      "quantile_0.90_rounded"
    )
  )
})

test_that("write_ref_date_summary_all includes designation and ensemble columns", {
  output_path <- write_ref_date_summary_all(
    reference_date = "2026-04-11",
    base_hub_path = example_cfa_hub,
    hub_reports_path = withr::local_tempdir(),
    disease = "covid",
    output_format = "csv",
    n_models_for_ens_reporting = 0
  )

  summary_data <- forecasttools::read_tabular(output_path)

  checkmate::expect_names(
    names(summary_data),
    must.include = c(
      "count_quantile_0.10",
      "count_quantile_0.90",
      "count_quantile_0.10_rounded",
      "count_quantile_0.90_rounded",
      "rate_quantile_0.10",
      "rate_quantile_0.90",
      "rate_quantile_0.10_rounded",
      "rate_quantile_0.90_rounded",
      "designated_model",
      "ensemble_of_hub_models"
    )
  )

  expect_false(any(
    stringr::str_detect(names(summary_data), "^quantile_|_per100k")
  ))

  ensemble_flags <- summary_data |>
    dplyr::filter(.data$model == "CovidHub-ensemble") |>
    dplyr::distinct(.data$designated_model, .data$ensemble_of_hub_models)
  expect_equal(nrow(ensemble_flags), 1)
  expect_false(ensemble_flags$designated_model[[1]])
  expect_true(ensemble_flags$ensemble_of_hub_models[[1]])

  epi_autogp_flags <- summary_data |>
    dplyr::filter(.data$model == "CFA-EpiAutoGP") |>
    dplyr::distinct(.data$target, .data$designated_model) |>
    dplyr::arrange(.data$target)
  expected_designation <- get_model_designation(
    base_hub_path = example_cfa_hub,
    model_ids = "CFA-EpiAutoGP",
    targets = epi_autogp_flags$target
  ) |>
    dplyr::arrange(.data$target)

  expect_identical(
    epi_autogp_flags$designated_model,
    expected_designation$designated
  )
})

test_that("write_ref_date_summary_all rate columns rescale counts by population", {
  output_path <- write_ref_date_summary_all(
    reference_date = "2026-04-11",
    base_hub_path = example_cfa_hub,
    hub_reports_path = withr::local_tempdir(),
    disease = "covid",
    output_format = "csv",
    n_models_for_ens_reporting = 0
  )

  summary_data <- forecasttools::read_tabular(output_path) |>
    dplyr::mutate(
      population = forecasttools::get_prism_reference_population(
        .data$abbreviation,
        as_of = as.Date("2026-04-11")
      )
    )

  hosp_data <- dplyr::filter(summary_data, .data$target_data_type == "hosp")
  expect_gt(nrow(hosp_data), 0)
  expect_equal(
    hosp_data$rate_quantile_0.5,
    hosp_data$count_quantile_0.5 /
      as.numeric(hosp_data$population) *
      100000
  )
  ed_data <- dplyr::filter(summary_data, .data$target_data_type == "prop_ed")
  expect_gt(nrow(ed_data), 0)
  expect_true(all(is.na(ed_data$rate_quantile_0.5)))
  expect_true(all(is.na(ed_data$rate_quantile_0.5_rounded)))
  expect_false(any(is.na(ed_data$count_quantile_0.5)))
})

test_that("write_ref_date_summary_ens uses count_/rate_ prefixed columns", {
  output_path <- write_ref_date_summary_ens(
    reference_date = "2026-04-11",
    base_hub_path = example_cfa_hub,
    hub_reports_path = withr::local_tempdir(),
    disease = "covid",
    output_format = "csv",
    n_models_for_ens_reporting = 0
  )

  map_data <- forecasttools::read_tabular(output_path)

  checkmate::expect_names(
    names(map_data),
    must.include = c(
      "count_quantile_0.025",
      "count_quantile_0.5",
      "count_quantile_0.975",
      "count_quantile_0.025_rounded",
      "count_quantile_0.5_rounded",
      "count_quantile_0.975_rounded",
      "rate_quantile_0.025",
      "rate_quantile_0.5",
      "rate_quantile_0.975",
      "rate_quantile_0.025_rounded",
      "rate_quantile_0.5_rounded",
      "rate_quantile_0.975_rounded"
    )
  )

  expect_false(any(
    stringr::str_detect(names(map_data), "^quantile_|_per100k")
  ))
})
