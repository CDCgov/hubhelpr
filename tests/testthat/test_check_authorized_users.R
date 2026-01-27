# Tests for check_authorized_users
#
# This test file validates the check_authorized_users function, which checks
# whether a GitHub user is authorized to modify specific model directories.
#
# Testing strategy:
# - All tests use testthat::local_mocked_bindings to mock hubData::load_model_metadata
#   to avoid needing to create complex hub metadata fixtures
# - Since the function works with model IDs and doesn't examine actual files,
#   no temporary directories or files are created
# - Tests validate both success cases (authorized users) and various error
#   cases (unauthorized users, missing metadata, etc.)
# - Tests verify that error messages are informative and include relevant details

test_that("check_authorized_users succeeds when user authorized for all models", {
  base_hub_path <- "/fake/hub/path"
  changed_model_ids <- c("team1-model", "team2-model")

  # Mock hubData::load_model_metadata
  local_mocked_bindings(
    load_model_metadata = function(hub_path) {
      tibble::tibble(
        model_id = c("team1-model", "team2-model"),
        designated_github_users = list(
          c("user1", "user2"),
          c("user1", "user3")
        )
      )
    },
    .package = "hubData"
  )

  # Should succeed without error
  expect_message(
    check_authorized_users(
      changed_model_ids = changed_model_ids,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Success: Changes authorized for user 'user1'"
  )
})

test_that("check_authorized_users errors when user not authorized for any model", {
  base_hub_path <- "/fake/hub/path"
  changed_model_ids <- c("team1-model")

  # Mock hubData::load_model_metadata
  local_mocked_bindings(
    load_model_metadata = function(hub_path) {
      tibble::tibble(
        model_id = "team1-model",
        designated_github_users = list(c("user1", "user2"))
      )
    },
    .package = "hubData"
  )

  expect_error(
    check_authorized_users(
      changed_model_ids = changed_model_ids,
      gh_actor = "unauthorized-user",
      base_hub_path = base_hub_path
    ),
    "Authorization check failed for user 'unauthorized-user'"
  )
})

test_that("check_authorized_users errors when user authorized for some but not all models", {
  base_hub_path <- "/fake/hub/path"
  changed_model_ids <- c("team1-model", "team2-model")

  # Mock hubData::load_model_metadata
  # user1 authorized for team1-model but not team2-model
  local_mocked_bindings(
    load_model_metadata = function(hub_path) {
      tibble::tibble(
        model_id = c("team1-model", "team2-model"),
        designated_github_users = list(
          c("user1", "user2"),
          c("other-user")
        )
      )
    },
    .package = "hubData"
  )

  expect_error(
    check_authorized_users(
      changed_model_ids = changed_model_ids,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Authorization check failed for user 'user1'"
  )

  # Verify error message includes details about unauthorized model
  expect_error(
    check_authorized_users(
      changed_model_ids = changed_model_ids,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "team2-model"
  )
})

test_that("check_authorized_users errors when model has no authorized users", {
  base_hub_path <- "/fake/hub/path"
  changed_model_ids <- c("team1-model")

  # Mock hubData::load_model_metadata with no designated users
  local_mocked_bindings(
    load_model_metadata = function(hub_path) {
      tibble::tibble(
        model_id = "team1-model",
        designated_github_users = list(character(0))
      )
    },
    .package = "hubData"
  )

  expect_error(
    check_authorized_users(
      changed_model_ids = changed_model_ids,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "no authorized users listed for that model"
  )
})

test_that("check_authorized_users errors when model directory not found in metadata", {
  base_hub_path <- "/fake/hub/path"
  changed_model_ids <- c("unknown-model")

  # Mock hubData::load_model_metadata with different models
  local_mocked_bindings(
    load_model_metadata = function(hub_path) {
      tibble::tibble(
        model_id = "team1-model",
        designated_github_users = list(c("user1", "user2"))
      )
    },
    .package = "hubData"
  )

  expect_error(
    check_authorized_users(
      changed_model_ids = changed_model_ids,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "cannot be modified in auto-approved PRs"
  )
})

test_that("check_authorized_users succeeds with single authorized model", {
  base_hub_path <- "/fake/hub/path"
  changed_model_ids <- c("team1-model")

  # Mock hubData::load_model_metadata
  local_mocked_bindings(
    load_model_metadata = function(hub_path) {
      tibble::tibble(
        model_id = "team1-model",
        designated_github_users = list(c("user1"))
      )
    },
    .package = "hubData"
  )

  expect_message(
    check_authorized_users(
      changed_model_ids = changed_model_ids,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Success: Changes authorized for user 'user1'"
  )
})

test_that("check_authorized_users handles NA values in designated_github_users", {
  base_hub_path <- "/fake/hub/path"
  changed_model_ids <- c("team1-model")

  # Mock hubData::load_model_metadata with NA values
  local_mocked_bindings(
    load_model_metadata = function(hub_path) {
      tibble::tibble(
        model_id = "team1-model",
        designated_github_users = list(c("user1", NA_character_, "user2"))
      )
    },
    .package = "hubData"
  )

  expect_message(
    check_authorized_users(
      changed_model_ids = changed_model_ids,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Success: Changes authorized for user 'user1'"
  )
})
