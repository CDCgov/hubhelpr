covidhub_path <- system.file(
  "testhubs/covidhub",
  package = "hubhelpr"
)
covidhub_targets <- c(
  "wk inc covid hosp",
  "wk inc covid prop ed visits"
)

test_that("get_model_designation designates all targets when designated_targets is absent", {
  designation <- get_model_designation(
    covidhub_path,
    model_ids = "CFA_Pyrenew-PyrenewHEW_COVID",
    targets = covidhub_targets
  )
  expect_equal(nrow(designation), 2)
  expect_true(all(designation$designated_model))
})

test_that("get_model_designation returns FALSE for all targets when designated_model is FALSE", {
  designation <- get_model_designation(
    covidhub_path,
    model_ids = "CFA_Pyrenew-Pyrenew_E_COVID",
    targets = covidhub_targets
  )
  expect_false(any(designation$designated_model))
})

test_that("get_model_designation narrows to only targets listed in designated_targets", {
  designation <- get_model_designation(
    covidhub_path,
    model_ids = "CFA-EpiAutoGP",
    targets = covidhub_targets
  ) |>
    dplyr::arrange(.data$target)
  expect_identical(designation$designated_model, c(TRUE, FALSE))
})

test_that("get_model_designation returns a full model-target grid, including FALSE rows", {
  designation <- get_model_designation(
    covidhub_path,
    model_ids = c(
      "CFA_Pyrenew-PyrenewHEW_COVID",
      "CovidHub-baseline"
    ),
    targets = covidhub_targets
  )
  expect_equal(nrow(designation), 4)
  baseline_rows <- designation$designated_model[
    designation$model_id == "CovidHub-baseline"
  ]
  hew_rows <- designation$designated_model[
    designation$model_id == "CFA_Pyrenew-PyrenewHEW_COVID"
  ]
  expect_false(any(baseline_rows))
  expect_true(all(hew_rows))
})

test_that("get_model_designation resolves a mix of broadly-, narrowly-, and non-designated models", {
  designation <- get_model_designation(
    covidhub_path,
    model_ids = c(
      "CFA-EpiAutoGP",
      "CFA_Pyrenew-PyrenewHEW_COVID",
      "CovidHub-baseline"
    ),
    targets = covidhub_targets
  ) |>
    dplyr::arrange(.data$model_id, .data$target)
  expect_equal(nrow(designation), 6)
  expect_identical(
    unname(designation$designated_model),
    c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("get_model_designation loads all hub models when model_ids is NULL", {
  designation <- get_model_designation(
    covidhub_path,
    targets = covidhub_targets
  )
  expect_equal(
    nrow(designation),
    8 * length(covidhub_targets)
  )
})

test_that("get_model_designation uses all hub-supported targets when targets is NULL", {
  designation <- get_model_designation(
    covidhub_path,
    model_ids = "CFA-EpiAutoGP"
  )
  expect_equal(nrow(designation), length(covidhub_targets))
  expect_setequal(designation$target, covidhub_targets)
})
