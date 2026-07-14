test_that("summarize_ref_date_forecasts includes 0.10 and 0.90 quantiles", {
  summary_data <- summarize_ref_date_forecasts(
    reference_date = "2026-04-11",
    base_hub_path = example_cfa_hub,
    disease = "covid",
    population_data = hubhelpr::population_data
  )

  expect_true(all(
    c(
      "quantile_0.10",
      "quantile_0.90",
      "quantile_0.10_rounded",
      "quantile_0.90_rounded"
    ) %in%
      names(summary_data)
  ))
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

  expect_true(all(
    c(
      "quantile_0.10",
      "quantile_0.90",
      "quantile_0.10_rounded",
      "quantile_0.90_rounded",
      "designated_model",
      "ensemble_of_hub_models"
    ) %in%
      names(summary_data)
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
