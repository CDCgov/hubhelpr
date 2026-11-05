## a hub copy whose time series carries missing observations, so the
## drop can be exercised end to end rather than in isolation
local_hub_with_missing_observations <- function(
  n_missing = 5L,
  env = parent.frame()
) {
  ## dir_copy places the source contents directly into the
  ## destination, so the copied hub root is the temp directory itself
  hub <- withr::local_tempdir(.local_envir = env)
  fs::dir_copy(example_cfa_hub, hub, overwrite = TRUE)

  series_path <- fs::path(hub, "target-data", "time-series.parquet")
  series <- arrow::read_parquet(series_path)
  latest <- max(series$as_of)
  rows <- which(
    series$as_of == latest & !is.na(series$observation)
  )[seq_len(n_missing)]
  series$observation[rows] <- NA_real_
  arrow::write_parquet(series, series_path)

  return(list(hub = hub, as_of = latest, n_missing = n_missing))
}

written_values <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)$value
}

test_that("observed data drops rows with a missing observation by default", {
  fixture <- local_hub_with_missing_observations()
  reports <- withr::local_tempdir()

  path <- write_viz_target_data(
    reference_date = as.character(fixture$as_of),
    base_hub_path = fixture$hub,
    hub_reports_path = reports,
    disease = "covid",
    use_hub_data = TRUE,
    as_of = fixture$as_of
  )

  expect_identical(sum(is.na(written_values(path))), 0L)
})

test_that("missing observations are kept when the drop is disabled", {
  fixture <- local_hub_with_missing_observations()
  reports <- withr::local_tempdir()

  path <- write_viz_target_data(
    reference_date = as.character(fixture$as_of),
    base_hub_path = fixture$hub,
    hub_reports_path = reports,
    disease = "covid",
    use_hub_data = TRUE,
    as_of = fixture$as_of,
    drop_missing_observations = FALSE
  )

  expect_identical(sum(is.na(written_values(path))), fixture$n_missing)
})

test_that("dropping missing observations removes only those rows", {
  fixture <- local_hub_with_missing_observations()
  kept <- withr::local_tempdir()
  dropped <- withr::local_tempdir()

  with_na <- write_viz_target_data(
    reference_date = as.character(fixture$as_of),
    base_hub_path = fixture$hub,
    hub_reports_path = kept,
    disease = "covid",
    use_hub_data = TRUE,
    as_of = fixture$as_of,
    drop_missing_observations = FALSE
  )
  without_na <- write_viz_target_data(
    reference_date = as.character(fixture$as_of),
    base_hub_path = fixture$hub,
    hub_reports_path = dropped,
    disease = "covid",
    use_hub_data = TRUE,
    as_of = fixture$as_of
  )

  expect_identical(
    nrow(readr::read_csv(with_na, show_col_types = FALSE)) -
      nrow(readr::read_csv(without_na, show_col_types = FALSE)),
    fixture$n_missing
  )
})

test_that("a non-logical drop argument is rejected", {
  expect_error(
    write_viz_target_data(
      reference_date = "2026-04-18",
      base_hub_path = example_cfa_hub,
      hub_reports_path = withr::local_tempdir(),
      disease = "covid",
      drop_missing_observations = "yes"
    )
  )
})
