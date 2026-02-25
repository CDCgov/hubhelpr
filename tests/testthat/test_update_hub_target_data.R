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
      disease = "measles"
    ),
    "Must be element of set \\{'covid','rsv'\\}"
  )
})

# test which exercises the full update_hub_target_data
# pipeline twice using mocked API responses; the first
# call writes target data to a temp hub directory; the
# second call with the same as_of date should error
# because the existing ows conflict with the incoming
# data; the third call with overwrite_existing = TRUE
# succeeding would confirm the parameter get through.
purrr::walk(c("covid", "rsv"), function(disease) {
  test_that(
    glue::glue(
      "update_hub_target_data errors on duplicate run for {disease}"
    ),
    {
      base_hub_path <- withr::local_tempdir(
        paste0("base_hub_dup_", disease, "_")
      )
      fs::dir_create(fs::path(base_hub_path, "target-data"))

      httptest2::with_mock_dir(mockdir_tests, {
        # first run succeeds
        hubhelpr::update_hub_target_data(
          base_hub_path = base_hub_path,
          disease = disease,
          as_of = lubridate::as_date("2025-08-18")
        )

        # second run :with same data errors by default
        expect_error(
          hubhelpr::update_hub_target_data(
            base_hub_path = base_hub_path,
            disease = disease,
            as_of = lubridate::as_date("2025-08-18")
          ),
          "overwrite"
        )

        # second run with overwrite_existing = TRUE
        # (should succeed)
        hubhelpr::update_hub_target_data(
          base_hub_path = base_hub_path,
          disease = disease,
          as_of = lubridate::as_date("2025-08-18"),
          overwrite_existing = TRUE
        )
      })
    }
  )
})


# mocked NHSN COVID response in hubverse format
httptest2::with_mock_dir(mockdir_tests, {
  nhsn_mock <- hubhelpr::get_hubverse_format_nhsn_data(
    disease = "covid",
    as_of = lubridate::as_date("2025-08-18"),
    start_date = lubridate::as_date("2024-11-09")
  )
})

# 2 locs, most recent date in data
real_td <- nhsn_mock |>
  dplyr::filter(
    .data$location %in% c("01", "02"),
    .data$date == max(.data$date)
  )

second_as_of <- lubridate::as_date("2025-08-25")

test_that("merge_target_data with NULL existing returns new data", {
  result <- merge_target_data(NULL, real_td)
  expect_equal(result, real_td)
})

test_that("merge_target_data appends non-overlapping vintages", {
  existing <- real_td
  new_data <- real_td |>
    dplyr::mutate(as_of = second_as_of)
  result <- merge_target_data(existing, new_data)
  expect_equal(nrow(result), nrow(existing) + nrow(new_data))
  expect_setequal(
    unique(result$as_of),
    c(real_td$as_of[1], second_as_of)
  )
})

test_that("merge_target_data errors on conflict by default", {
  existing <- real_td
  new_data <- real_td |>
    dplyr::mutate(observation = observation + 1)
  expect_error(
    merge_target_data(existing, new_data),
    "overwrite"
  )
})

test_that("merge_target_data overwrites only shared rows when TRUE", {
  # create existing data with two targets using real
  # nhsn data for hosp and a derived ed target
  hosp_td <- real_td
  ed_td <- real_td |>
    dplyr::mutate(
      target = "wk inc covid prop ed visits",
      observation = 0.05
    )
  existing <- dplyr::bind_rows(hosp_td, ed_td)

  # update ED with diff values; not hosp
  new_ed <- ed_td |>
    dplyr::mutate(observation = 0.07)

  result <- merge_target_data(
    existing,
    new_ed,
    overwrite_existing = TRUE
  )

  hosp_rows <- dplyr::filter(
    result,
    hubhelpr::is_hosp_target(.data$target)
  )
  expect_equal(nrow(hosp_rows), nrow(hosp_td))
  expect_equal(hosp_rows$observation, hosp_td$observation)

  ed_rows <- dplyr::filter(
    result,
    hubhelpr::is_ed_target(.data$target)
  )
  expect_equal(nrow(ed_rows), nrow(ed_td))
  expect_true(all(ed_rows$observation == 0.07))
})

test_that("merge_target_data overwrite with identical data has no duplicates", {
  result <- merge_target_data(real_td, real_td, overwrite_existing = TRUE)
  expect_equal(nrow(result), nrow(real_td))
})

# verifies that merge_target_data treats pre-existing
# duplicate rows as a data issue rather than silently
# deduplicating them; existing data is (intended)
# doubled via bind_rows, and the function should abort
# with a message (even though the new data itself
# doesnt overlap with the duplicated keys)
test_that("merge_target_data errors on duplicate rows in existing data", {
  existing <- dplyr::bind_rows(real_td, real_td)
  new_data <- real_td |>
    dplyr::mutate(as_of = second_as_of)
  expect_error(
    merge_target_data(existing, new_data),
    "Duplicate rows found"
  )
})

# verifies that the error message from merge_target_data
# includes the exact number of rows that would be
# overwritten. the row count in the message should match
# the number of rows in the real data slice that share
# key columns with the new data.
test_that("merge_target_data reports correct overwrite count", {
  existing <- real_td
  new_data <- real_td |>
    dplyr::mutate(observation = observation + 1)
  n_conflicts <- nrow(real_td)
  expect_error(
    merge_target_data(existing, new_data),
    glue::glue("{n_conflicts} row")
  )
})

# verifies that after overwrite, the resulting data
# frame retains the expected hubverse time-series column
# structure and that the observation values come entirely
# from the new data.
test_that("merge_target_data preserves all columns after overwrite", {
  existing <- real_td
  new_data <- real_td |>
    dplyr::mutate(observation = observation + 1)
  result <- merge_target_data(existing, new_data, overwrite_existing = TRUE)
  expect_equal(
    names(result),
    c("date", "observation", "location", "as_of", "target")
  )
  expect_equal(result$observation, new_data$observation)
})
