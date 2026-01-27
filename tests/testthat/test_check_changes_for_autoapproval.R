# Tests for check_changes_for_autoapproval
#
# This test file validates the check_changes_for_autoapproval function,
# which is used in GitHub workflows to validate that PR changes are eligible
# for auto-approval.
#
# Testing strategy:
# - Tests that don't require authorization checking can run without mocking
# - Tests that require authorization checking use testthat::local_mocked_bindings
#   to mock hubData::load_model_metadata, avoiding the need for complex hub fixtures
# - Since the function works with file paths and doesn't examine actual files,
#   no temporary directories or files are created
# - Tests validate both success cases (authorized changes) and error cases
#   (unauthorized changes, files outside model-output, etc.)

test_that("check_changes_for_autoapproval succeeds with valid changes to model-output", {
  base_hub_path <- "/fake/hub/path"
  changed_files <- c("/fake/hub/path/model-output/team1-model/test.csv")

  # Mock hubData::load_model_metadata to return expected structure
  local_mocked_bindings(
    load_model_metadata = function(hub_path) {
      tibble::tibble(
        model_id = "team1-model",
        designated_github_users = list(c("user1", "user2"))
      )
    },
    .package = "hubData"
  )

  # Should not raise an error for authorized user
  expect_silent(
    check_changes_for_autoapproval(
      changed_files = changed_files,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    )
  )
})

test_that("check_changes_for_autoapproval errors when no changed files", {
  base_hub_path <- "/fake/hub/path"

  expect_error(
    check_changes_for_autoapproval(
      changed_files = character(0),
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Empty PRs cannot be autoapproved"
  )
})

test_that("check_changes_for_autoapproval errors when files outside model-output", {
  base_hub_path <- "/fake/hub/path"
  changed_files <- c("/fake/hub/path/other-dir/test.txt")

  expect_error(
    check_changes_for_autoapproval(
      changed_files = changed_files,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Auto-approval failed: Changes detected outside 'model-output' directory"
  )
})

test_that("check_changes_for_autoapproval errors when files both inside and outside model-output", {
  base_hub_path <- "/fake/hub/path"
  changed_files <- c(
    "/fake/hub/path/model-output/team1-model/test.csv",
    "/fake/hub/path/other-dir/test.txt"
  )

  expect_error(
    check_changes_for_autoapproval(
      changed_files = changed_files,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Auto-approval failed: Changes detected outside 'model-output' directory"
  )
})

test_that("check_changes_for_autoapproval errors when user not authorized for any model", {
  base_hub_path <- "/fake/hub/path"
  changed_files <- c("/fake/hub/path/model-output/team1-model/test.csv")

  # Mock hubData::load_model_metadata to return model with different authorized users
  local_mocked_bindings(
    load_model_metadata = function(hub_path) {
      tibble::tibble(
        model_id = "team1-model",
        designated_github_users = list(c("other-user1", "other-user2"))
      )
    },
    .package = "hubData"
  )

  expect_error(
    check_changes_for_autoapproval(
      changed_files = changed_files,
      gh_actor = "unauthorized-user",
      base_hub_path = base_hub_path
    ),
    "Authorization check failed for user 'unauthorized-user'"
  )
})

test_that("check_changes_for_autoapproval errors when user authorized for some models but not all", {
  base_hub_path <- "/fake/hub/path"
  changed_files <- c(
    "/fake/hub/path/model-output/team1-model/test1.csv",
    "/fake/hub/path/model-output/team2-model/test2.csv"
  )

  # Mock hubData::load_model_metadata
  # user1 is authorized for team1-model but not team2-model
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
    check_changes_for_autoapproval(
      changed_files = changed_files,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Authorization check failed for user 'user1'"
  )
})

test_that("check_changes_for_autoapproval succeeds when user authorized for multiple models", {
  base_hub_path <- "/fake/hub/path"
  changed_files <- c(
    "/fake/hub/path/model-output/team1-model/test1.csv",
    "/fake/hub/path/model-output/team2-model/test2.csv"
  )

  # Mock hubData::load_model_metadata
  # user1 is authorized for both models
  local_mocked_bindings(
    load_model_metadata = function(hub_path) {
      tibble::tibble(
        model_id = c("team1-model", "team2-model"),
        designated_github_users = list(
          c("user1", "user2"),
          c("user1", "other-user")
        )
      )
    },
    .package = "hubData"
  )

  expect_silent(
    check_changes_for_autoapproval(
      changed_files = changed_files,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    )
  )
})
