test_that("reference dates before the minimum was introduced require no models", {
  # the hub reported its ensemble unconditionally until
  # 2026-04-04, so regenerating those weeks under today's minimum
  # would drop rows that were legitimately published
  expect_identical(n_models_for_ens_reporting_as_of("2024-11-16"), 0L)
  expect_identical(n_models_for_ens_reporting_as_of("2026-03-28"), 0L)
})

test_that("reference dates from 2026-04-04 onward require two models", {
  expect_identical(n_models_for_ens_reporting_as_of("2026-04-04"), 2L)
  expect_identical(n_models_for_ens_reporting_as_of("2026-08-22"), 2L)
})

test_that("the minimum changes on the reference date it took effect, not before", {
  expect_identical(n_models_for_ens_reporting_as_of("2026-03-28"), 0L)
  expect_identical(n_models_for_ens_reporting_as_of("2026-04-04"), 2L)
})

test_that("reference dates are looked up identically as Date or character", {
  expect_identical(
    n_models_for_ens_reporting_as_of(as.Date("2026-04-04")),
    n_models_for_ens_reporting_as_of("2026-04-04")
  )
})

test_that("a reference date before the record begins is an error", {
  # returning a minimum for a week the record says nothing about
  # would be a guess presented as a fact
  expect_error(
    n_models_for_ens_reporting_as_of("2020-01-04"),
    "No ensemble reporting minimum recorded"
  )
})

test_that("the record is ordered, so the latest applicable row wins", {
  expect_identical(
    hubhelpr:::ensemble_reporting_minimums$reference_date,
    sort(hubhelpr:::ensemble_reporting_minimums$reference_date)
  )
})
