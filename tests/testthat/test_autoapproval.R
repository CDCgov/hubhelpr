test_that("check_authorized_users succeeds for an authorized user", {
  expect_invisible(
    hubhelpr::check_authorized_users(
      changed_model_ids = "CFA-EpiAutoGP",
      gh_actor = "SamuelBrand1",
      base_hub_path = example_cfa_hub
    )
  )
})

test_that("check_authorized_users succeeds across multiple model dirs", {
  expect_invisible(
    hubhelpr::check_authorized_users(
      changed_model_ids = c(
        "CFA_Pyrenew-PyrenewHEW_COVID",
        "CFA_Pyrenew-Pyrenew_H_COVID"
      ),
      gh_actor = "sbidari",
      base_hub_path = example_cfa_hub
    )
  )
})

test_that("check_authorized_users errors for an unauthorized user", {
  expect_error(
    hubhelpr::check_authorized_users(
      changed_model_ids = "CFA-EpiAutoGP",
      gh_actor = "not-a-real-user",
      base_hub_path = example_cfa_hub
    ),
    regexp = "Authorization check failed"
  )
})


test_that("check_authorized_users errors when an actor authorized for
 a model dir makes changes to a different model dir", {
  expect_error(
    hubhelpr::check_authorized_users(
      changed_model_ids = "CFA-EpiAutoGP",
      gh_actor = "dylanhmorris",
      base_hub_path = example_cfa_hub
    ),
    regexp = "Authorization check failed"
  )
})


test_that("check_authorized_users errors when model has no metadata", {
  expect_error(
    hubhelpr::check_authorized_users(
      changed_model_ids = "NonExistent-Model",
      gh_actor = "someuser",
      base_hub_path = example_cfa_hub
    ),
    regexp = "Authorization check failed"
  )
})


test_that("check_changes_for_autoapproval succeeds for authorized
user modifying one model dir", {
  changed_files <- fs::path(
    example_cfa_hub,
    "model-output",
    "CFA-EpiAutoGP",
    "2026-04-18-CFA-EpiAutoGP.csv"
  )
  expect_invisible(
    hubhelpr::check_changes_for_autoapproval(
      changed_files = changed_files,
      gh_actor = "SamuelBrand1",
      base_hub_path = example_cfa_hub
    )
  )
})

test_that("check_changes_for_autoapproval succeeds for authorized user modifying multiple files in one model dir", {
  changed_files <- fs::path(
    example_cfa_hub,
    "model-output",
    "CFA-EpiAutoGP",
    c("2026-04-11-CFA-EpiAutoGP.csv", "2026-04-18-CFA-EpiAutoGP.csv")
  )
  expect_invisible(
    hubhelpr::check_changes_for_autoapproval(
      changed_files = changed_files,
      gh_actor = "SamuelBrand1",
      base_hub_path = example_cfa_hub
    )
  )
})

test_that("check_changes_for_autoapproval errors for empty changed_files", {
  expect_error(
    hubhelpr::check_changes_for_autoapproval(
      changed_files = character(0),
      gh_actor = "SamuelBrand1",
      base_hub_path = example_cfa_hub
    ),
    regexp = "Empty PRs cannot be autoapproved"
  )
})

test_that("check_changes_for_autoapproval errors when files
are outside model-output", {
  changed_files <- c(
    fs::path(
      example_cfa_hub,
      "model-output",
      "CFA-EpiAutoGP",
      "2026-04-18-CFA-EpiAutoGP.csv"
    ),
    fs::path(example_cfa_hub, "hub-config", "tasks.json")
  )
  expect_error(
    hubhelpr::check_changes_for_autoapproval(
      changed_files = changed_files,
      gh_actor = "SamuelBrand1",
      base_hub_path = example_cfa_hub
    ),
    regexp = "Changes detected outside 'model-output' directory"
  )
})

test_that("check_changes_for_autoapproval errors for unauthorized user", {
  changed_files <- fs::path(
    example_cfa_hub,
    "model-output",
    "CFA-EpiAutoGP",
    "2026-04-18-CFA-EpiAutoGP.csv"
  )
  expect_error(
    hubhelpr::check_changes_for_autoapproval(
      changed_files = changed_files,
      gh_actor = "not-a-real-user",
      base_hub_path = example_cfa_hub
    ),
    regexp = "Authorization check failed for user 'not-a-real-user'"
  )
})


test_that("check_changes_for_autoapproval errors when an actor
changes a model-output dir they are not authorized for", {
  changed_files <- c(
    fs::path(
      example_cfa_hub,
      "model-output",
      "CFA-EpiAutoGP",
      "2026-04-18-CFA-EpiAutoGP.csv"
    ),
    fs::path(
      example_cfa_hub,
      "model-output",
      "CFA_Pyrenew-PyrenewHEW_COVID",
      "2026-04-18-CFA_Pyrenew-PyrenewHEW_COVID.csv"
    )
  )

  expect_error(
    hubhelpr::check_changes_for_autoapproval(
      changed_files = changed_files,
      gh_actor = "sbidari",
      base_hub_path = example_cfa_hub
    ),
    regexp = "Authorization check failed"
  )
})
