library(fs)
library(dplyr)
library(testthat)
library(hubhelpr)
library(forecasttools)

run_update_hub_target_data_test <- function(
  disease,
  nhsn_col,
  nssp_col,
  nhsn_target,
  nssp_target
) {
  base_hub_path <- tempfile(paste0("base_hub_", disease, "_"))
  dir_create(base_hub_path)
  output_file <- path(base_hub_path, "target-data/time-series.parquet")
  fs::dir_create(fs::path(base_hub_path, "target-data"))

  mock_target_ts <- tibble::tibble(
    date = as.Date(character()),
    observation = numeric(),
    location = character(),
    as_of = as.Date(character()),
    target = character()
  )
  forecasttools::write_tabular_file(mock_target_ts, output_file)

  mock_nhsn <- tibble::tibble(
    weekendingdate = as.Date(c(
      "2024-11-09",
      "2024-11-16",
      "2024-11-09",
      "2024-11-16"
    )),
    jurisdiction = c("MI", "MI", "CA", "CA"),
    totalconfc19newadm = c(1, 2, 1, 0),
    totalconfrsvnewadm = c(3, 4, 3, 2)
  )
  mock_nssp <- tibble::tibble(
    week_end = as.Date(c("2024-11-09", "2024-11-09")),
    county = c("All", "All"),
    geography = c("Alabama", "Alaska"),
    percent_visits_covid = c(1.0, 0.2),
    percent_visits_rsv = c(0.5, 0.15)
  )
  assignInNamespace(
    "pull_data_cdc_gov_dataset",
    function(dataset, columns, start_date = NULL, locations = NULL) {
      if (dataset == "nhsn_hrd_prelim") {
        mock_nhsn |>
          dplyr::select(weekendingdate, jurisdiction, all_of(columns))
      } else if (dataset == "nssp_prop_ed_visits") {
        mock_nssp |> dplyr::select(week_end, geography, all_of(columns))
      } else {
        stop("Unknown dataset")
      }
    },
    ns = "forecasttools"
  )

  expect_silent(
    update_hub_target_data(
      base_hub_path = base_hub_path,
      disease = disease,
      first_full_weekending_date = as.Date("2024-11-09")
    )
  )

  target_ts <- forecasttools::read_tabular_file(output_file)

  expect_true(all(
    c("date", "observation", "location", "as_of", "target") %in%
      names(target_ts)
  ))

  expect_equal(
    target_ts |>
      dplyr::select(date, observation, location, target) |>
      dplyr::arrange(date, location),
    tibble::tibble(
      date = as.Date(c(mock_nhsn$weekendingdate, mock_nssp$week_end)),
      observation = c(
        as.numeric(mock_nhsn[[nhsn_col]]),
        as.numeric(mock_nssp[[nssp_col]]) / 100
      ),
      location = c(
        us_location_recode(mock_nhsn$jurisdiction, "abbr", "code"),
        us_location_recode(mock_nssp$geography, "name", "code")
      ),
      target = c(rep(nhsn_target, 4), rep(nssp_target, 2))
    ) |>
      dplyr::arrange(date, location)
  )
}

test_that("update_hub_target_data returns exactly the mocked NHSN data for covid", {
  run_update_hub_target_data_test(
    disease = "covid",
    nhsn_col = "totalconfc19newadm",
    nssp_col = "percent_visits_covid",
    nhsn_target = "wk inc covid hosp",
    nssp_target = "wk inc covid prop ed visits"
  )
})

test_that("update_hub_target_data returns exactly the mocked NHSN data for rsv", {
  run_update_hub_target_data_test(
    disease = "rsv",
    nhsn_col = "totalconfrsvnewadm",
    nssp_col = "percent_visits_rsv",
    nhsn_target = "wk inc rsv hosp",
    nssp_target = "wk inc rsv prop ed visits"
  )
})
