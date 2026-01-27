# Tests for check_changes_for_autoapproval
#
# This test file validates the check_changes_for_autoapproval function,
# which is used in GitHub workflows to validate that PR changes are eligible
# for auto-approval.
#
# Testing strategy:
# - Creates temporary hub directories with model-metadata YAML files
# - Uses real hubData::load_model_metadata() calls (not mocked) to read metadata
# - Tests validate both success cases (authorized changes) and error cases
#   (unauthorized changes, files outside model-output, etc.)

test_that("check_changes_for_autoapproval succeeds with valid changes to model-output", {
  # Create a temporary hub directory structure
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_metadata_dir <- fs::path(base_hub_path, "model-metadata")
  fs::dir_create(model_metadata_dir)
  
  # Create model metadata file for team1-model
  metadata_file <- fs::path(model_metadata_dir, "team1-model.yml")
  writeLines(c(
    "team_name: Team 1",
    "model_name: Model 1",
    "designated_github_users:",
    "  - user1",
    "  - user2"
  ), metadata_file)
  
  # File paths don't need to exist, just valid paths
  changed_files <- c(fs::path(base_hub_path, "model-output", "team1-model", "test.csv"))
  
  # Should not raise an error for authorized user
  expect_silent(
    check_changes_for_autoapproval(
      changed_files = as.character(changed_files),
      gh_actor = "user1",
      base_hub_path = base_hub_path
    )
  )
})

test_that("check_changes_for_autoapproval errors when no changed files", {
  base_hub_path <- withr::local_tempdir("test_hub_")
  
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
  base_hub_path <- withr::local_tempdir("test_hub_")
  changed_files <- c(fs::path(base_hub_path, "other-dir", "test.txt"))
  
  expect_error(
    check_changes_for_autoapproval(
      changed_files = as.character(changed_files),
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Auto-approval failed: Changes detected outside 'model-output' directory"
  )
})

test_that("check_changes_for_autoapproval errors when files both inside and outside model-output", {
  base_hub_path <- withr::local_tempdir("test_hub_")
  changed_files <- c(
    fs::path(base_hub_path, "model-output", "team1-model", "test.csv"),
    fs::path(base_hub_path, "other-dir", "test.txt")
  )
  
  expect_error(
    check_changes_for_autoapproval(
      changed_files = as.character(changed_files),
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Auto-approval failed: Changes detected outside 'model-output' directory"
  )
})

test_that("check_changes_for_autoapproval errors when user not authorized for any model", {
  # Create a temporary hub directory structure
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_metadata_dir <- fs::path(base_hub_path, "model-metadata")
  fs::dir_create(model_metadata_dir)
  
  # Create model metadata file with different authorized users
  metadata_file <- fs::path(model_metadata_dir, "team1-model.yml")
  writeLines(c(
    "team_name: Team 1",
    "model_name: Model 1",
    "designated_github_users:",
    "  - other-user1",
    "  - other-user2"
  ), metadata_file)
  
  changed_files <- c(fs::path(base_hub_path, "model-output", "team1-model", "test.csv"))
  
  expect_error(
    check_changes_for_autoapproval(
      changed_files = as.character(changed_files),
      gh_actor = "unauthorized-user",
      base_hub_path = base_hub_path
    ),
    "Authorization check failed for user 'unauthorized-user'"
  )
})

test_that("check_changes_for_autoapproval errors when user authorized for some models but not all", {
  # Create a temporary hub directory structure
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_metadata_dir <- fs::path(base_hub_path, "model-metadata")
  fs::dir_create(model_metadata_dir)
  
  # Create model metadata files
  # user1 is authorized for team1-model but not team2-model
  metadata_file1 <- fs::path(model_metadata_dir, "team1-model.yml")
  writeLines(c(
    "team_name: Team 1",
    "model_name: Model 1",
    "designated_github_users:",
    "  - user1",
    "  - user2"
  ), metadata_file1)
  
  metadata_file2 <- fs::path(model_metadata_dir, "team2-model.yml")
  writeLines(c(
    "team_name: Team 2",
    "model_name: Model 2",
    "designated_github_users:",
    "  - other-user"
  ), metadata_file2)
  
  changed_files <- c(
    fs::path(base_hub_path, "model-output", "team1-model", "test1.csv"),
    fs::path(base_hub_path, "model-output", "team2-model", "test2.csv")
  )
  
  expect_error(
    check_changes_for_autoapproval(
      changed_files = as.character(changed_files),
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Authorization check failed for user 'user1'"
  )
})

test_that("check_changes_for_autoapproval succeeds when user authorized for multiple models", {
  # Create a temporary hub directory structure
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_metadata_dir <- fs::path(base_hub_path, "model-metadata")
  fs::dir_create(model_metadata_dir)
  
  # Create model metadata files
  # user1 is authorized for both models
  metadata_file1 <- fs::path(model_metadata_dir, "team1-model.yml")
  writeLines(c(
    "team_name: Team 1",
    "model_name: Model 1",
    "designated_github_users:",
    "  - user1",
    "  - user2"
  ), metadata_file1)
  
  metadata_file2 <- fs::path(model_metadata_dir, "team2-model.yml")
  writeLines(c(
    "team_name: Team 2",
    "model_name: Model 2",
    "designated_github_users:",
    "  - user1",
    "  - other-user"
  ), metadata_file2)
  
  changed_files <- c(
    fs::path(base_hub_path, "model-output", "team1-model", "test1.csv"),
    fs::path(base_hub_path, "model-output", "team2-model", "test2.csv")
  )
  
  expect_silent(
    check_changes_for_autoapproval(
      changed_files = as.character(changed_files),
      gh_actor = "user1",
      base_hub_path = base_hub_path
    )
  )
})
