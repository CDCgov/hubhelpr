test_that("get_model_designation designates all targets when designated_targets is absent", {
  designation <- get_model_designation(
    example_cfa_hub,
    model_ids = "CFA_Pyrenew-PyrenewHEW_COVID"
  )
  expect_equal(nrow(designation), 2)
  expect_true(all(designation$designated))
})

test_that("get_model_designation returns FALSE for all targets when designated is FALSE", {
  designation <- get_model_designation(
    example_cfa_hub,
    model_ids = "CFA_Pyrenew-Pyrenew_E_COVID"
  )
  expect_false(any(designation$designated))
})

test_that("get_model_designation narrows to only targets listed in designated_targets", {
  designation <- get_model_designation(
    example_cfa_hub,
    model_ids = "CFA-EpiAutoGP"
  ) |>
    dplyr::arrange(.data$target)
  expect_identical(designation$designated, c(TRUE, FALSE))
})

test_that("get_model_designation returns a full model-target grid, including FALSE rows", {
  designation <- get_model_designation(
    example_cfa_hub,
    model_ids = c(
      "CFA_Pyrenew-PyrenewHEW_COVID",
      "CovidHub-baseline"
    )
  )
  expect_equal(nrow(designation), 4)
  baseline_rows <- designation$designated[
    designation$model_id == "CovidHub-baseline"
  ]
  hew_rows <- designation$designated[
    designation$model_id == "CFA_Pyrenew-PyrenewHEW_COVID"
  ]
  expect_false(any(baseline_rows))
  expect_true(all(hew_rows))
})

test_that("get_model_designation resolves a mix of broadly-, narrowly-, and non-designated models", {
  designation <- get_model_designation(
    example_cfa_hub,
    model_ids = c(
      "CFA-EpiAutoGP",
      "CFA_Pyrenew-PyrenewHEW_COVID",
      "CovidHub-baseline"
    )
  ) |>
    dplyr::arrange(.data$model_id, .data$target)
  expect_equal(nrow(designation), 6)
  expect_identical(
    unname(designation$designated),
    c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("get_model_designation loads all hub models when model_ids is NULL", {
  designation <- get_model_designation(example_cfa_hub)
  n_models <- hubData::load_model_metadata(example_cfa_hub) |>
    dplyr::distinct(.data$model_id) |>
    nrow()
  n_targets <- length(get_hub_supported_targets(example_cfa_hub))
  expect_equal(nrow(designation), n_models * n_targets)
})

test_that("get_model_designation works with a single target input", {
  designation <- get_model_designation(
    example_cfa_hub,
    model_ids = "CFA-EpiAutoGP",
    targets = "wk inc covid hosp"
  )
  expect_equal(nrow(designation), 1)
  expect_true(designation$designated)
})

test_that("get_model_designation uses all hub-supported targets when targets is NULL", {
  designation <- get_model_designation(
    example_cfa_hub,
    model_ids = "CFA-EpiAutoGP"
  )
  expected_targets <- get_hub_supported_targets(example_cfa_hub)
  expect_equal(nrow(designation), length(expected_targets))
  expect_setequal(designation$target, expected_targets)
})
