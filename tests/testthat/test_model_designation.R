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

# designation as of a reference date, read from the
# hub's weekly-model-submissions record rather than
# current metadata

hosp_target <- "wk inc covid hosp"
ed_target <- "wk inc covid prop ed visits"

designated_for <- function(designation, model_id, target) {
  designation$designated[
    designation$model_id == model_id & designation$target == target
  ]
}

test_that("designation for a reference date comes from the submissions record, not metadata", {
  from_metadata <- get_model_designation(
    example_cfa_hub,
    model_ids = "CovidHub-baseline"
  )
  from_record <- get_model_designation(
    example_cfa_hub,
    model_ids = "CovidHub-baseline",
    reference_date = "2025-12-06"
  )

  expect_false(any(from_metadata$designated))
  expect_true(all(from_record$designated))
})

test_that("a record with no target column designates across every target", {
  designation <- get_model_designation(
    example_cfa_hub,
    reference_date = "2025-12-06"
  )
  targets <- get_hub_supported_targets(example_cfa_hub)

  expect_setequal(designation$target, targets)
  expect_true(all(designated_for(designation, "CovidHub-baseline", targets)))
  expect_false(any(designated_for(designation, "CFA-EpiAutoGP", targets)))
})

test_that("a record with a target column designates per target", {
  designation <- get_model_designation(
    example_cfa_hub,
    reference_date = "2025-12-13"
  )

  expect_true(designated_for(designation, "CFA-EpiAutoGP", hosp_target))
  expect_false(designated_for(designation, "CFA-EpiAutoGP", ed_target))
  expect_true(designated_for(designation, "CovidHub-baseline", hosp_target))
  expect_false(designated_for(designation, "CovidHub-baseline", ed_target))
})

test_that("every schema generation of the record is read", {
  expectations <- list(
    "2025-12-06" = NULL,
    "2025-12-13" = TRUE,
    "2026-04-11" = FALSE,
    "2026-04-18" = TRUE
  )

  purrr::iwalk(expectations[-1], \(expected, reference_date) {
    designation <- get_model_designation(
      example_cfa_hub,
      reference_date = reference_date
    )
    expect_equal(
      designated_for(designation, "CFA-EpiAutoGP", hosp_target),
      expected,
      info = reference_date
    )
  })
})

test_that("models absent from the record are not designated for that week", {
  designation <- get_model_designation(
    example_cfa_hub,
    model_ids = c("CFA-EpiAutoGP", "CFA_Pyrenew-Pyrenew_H_COVID"),
    reference_date = "2026-04-18"
  )

  expect_true(all(
    designated_for(designation, "CFA-EpiAutoGP", c(hosp_target, ed_target))
  ))
  expect_false(any(
    designated_for(
      designation,
      "CFA_Pyrenew-Pyrenew_H_COVID",
      c(hosp_target, ed_target)
    )
  ))
})

test_that("designation falls back to metadata when the hub has no record for the date", {
  from_metadata <- get_model_designation(
    example_cfa_hub,
    model_ids = "CFA-EpiAutoGP"
  )
  no_record <- get_model_designation(
    example_cfa_hub,
    model_ids = "CFA-EpiAutoGP",
    reference_date = "2020-01-01"
  )

  expect_equal(no_record, from_metadata)
})
