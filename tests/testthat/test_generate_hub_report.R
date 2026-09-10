test_that("generate_hub_report() writes all expected files", {
  report_dir <- withr::local_tempdir()
  write_mock_ens_minimums_file(report_dir, "covid", "2026-04-11 = 0")
  reference_date <- "2026-04-18"
  expected_summary_dir <- fs::path(
    report_dir,
    "weekly-summaries",
    "covid19-forecast-hub",
    reference_date
  )
  expected_files <- purrr::map_chr(
    c("forecasts_data.csv", "map_data.csv", "target_data.csv", "webtext.md"),
    \(suffix) {
      fs::path(
        expected_summary_dir,
        glue::glue("{reference_date}_covid_{suffix}")
      )
    }
  )
  mockdir_reports <- fs::path(mockdir_tests, "report-generation")

  httptest2::with_mock_dir(mockdir_reports, {
    replace_env_vars_if_mocking(mockdir_reports)
    generate_hub_report(
      reference_date,
      "covid",
      report_dir,
      base_hub_path = example_cfa_hub,
      excluded_locations = "UM" # not in NHSN, excluded from real reports
    )
  })

  expect_true(fs::dir_exists(expected_summary_dir))
  expect_all_true(fs::file_exists(expected_files))
})
