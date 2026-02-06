mockdir_tests <- fs::path(mockdir)

## replace env variables with fakes if and only if
## we are mocking api calls
if (fs::dir_exists(mockdir_tests)) {
  withr::local_envvar(
    .new = c(
      "DATA_CDC_GOV_API_KEY_ID" = "fake_key",
      "DATA_CDC_GOV_API_KEY_SECRET" = "fake_secret" #pragma: allowlist secret
    )
  )
}

purrr::walk(c("covid", "rsv"), function(disease) {
  test_that(
    glue::glue("update_hub_target_data returns expected data for {disease}"),
    {
      base_hub_path <- withr::local_tempdir(paste0("base_hub_", disease, "_"))
      output_file <- fs::path(base_hub_path, "target-data/time-series.parquet")
      fs::dir_create(fs::path(base_hub_path, "target-data"))

      httptest2::with_mock_dir(mockdir_tests, {
        hubhelpr::update_hub_target_data(
          base_hub_path = base_hub_path,
          disease = disease,
          as_of = lubridate::as_date("2025-08-18"),
        )

        target_ts <- forecasttools::read_tabular_file(output_file)
        expect_equal(
          names(target_ts),
          c("date", "observation", "location", "as_of", "target")
        )
        expect_setequal(
          unique(target_ts$target),
          c(
            glue::glue("wk inc {disease} prop ed visits"),
            glue::glue("wk inc {disease} hosp")
          )
        )
        expect_setequal(
          unique(target_ts$location),
          setdiff(forecasttools::us_location_table$code, excluded_locations)
        )
      })
    }
  )
})

test_that("update_hub_target_data errors for unsupported disease", {
  expect_error(
    update_hub_target_data(
      base_hub_path = tempdir(),
      disease = "flu"
    ),
    "Assertion on 'disease' failed"
  )
})
