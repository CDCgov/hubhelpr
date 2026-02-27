# Tests for check_authorized_users
#
# This test file validates the check_authorized_users function, which checks
# whether a GitHub user is authorized to modify specific model directories.
#
# Testing strategy:
# - Creates temporary hub directories with model-metadata YAML files
# - Uses real hubData::load_model_metadata() calls (not mocked) to read metadata
# - Tests validate both success cases (authorized users) and various error
#   cases (unauthorized users, missing metadata, etc.)
# - Tests verify that error messages are informative and include relevant details

test_that("check_authorized_users succeeds when user authorized for all models", {
  # Create a temporary hub directory structure
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_metadata_dir <- fs::path(base_hub_path, "model-metadata")
  fs::dir_create(model_metadata_dir)

  # Create model metadata files
  metadata_file1 <- fs::path(model_metadata_dir, "team1-model.yml")
  writeLines(
    c(
      "team_name: Team 1",
      "model_name: Model 1",
      "designated_github_users:",
      "  - user1",
      "  - user2"
    ),
    metadata_file1
  )

  metadata_file2 <- fs::path(model_metadata_dir, "team2-model.yml")
  writeLines(
    c(
      "team_name: Team 2",
      "model_name: Model 2",
      "designated_github_users:",
      "  - user1",
      "  - user3"
    ),
    metadata_file2
  )

  changed_model_ids <- c("team1-model", "team2-model")

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
  # Create a temporary hub directory structure
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_metadata_dir <- fs::path(base_hub_path, "model-metadata")
  fs::dir_create(model_metadata_dir)

  # Create model metadata file
  metadata_file <- fs::path(model_metadata_dir, "team1-model.yml")
  writeLines(
    c(
      "team_name: Team 1",
      "model_name: Model 1",
      "designated_github_users:",
      "  - user1",
      "  - user2"
    ),
    metadata_file
  )

  changed_model_ids <- c("team1-model")

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
  # Create a temporary hub directory structure
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_metadata_dir <- fs::path(base_hub_path, "model-metadata")
  fs::dir_create(model_metadata_dir)

  # Create model metadata files
  # user1 authorized for team1-model but not team2-model
  metadata_file1 <- fs::path(model_metadata_dir, "team1-model.yml")
  writeLines(
    c(
      "team_name: Team 1",
      "model_name: Model 1",
      "designated_github_users:",
      "  - user1",
      "  - user2"
    ),
    metadata_file1
  )

  metadata_file2 <- fs::path(model_metadata_dir, "team2-model.yml")
  writeLines(
    c(
      "team_name: Team 2",
      "model_name: Model 2",
      "designated_github_users:",
      "  - other-user"
    ),
    metadata_file2
  )

  changed_model_ids <- c("team1-model", "team2-model")

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
  # Create a temporary hub directory structure
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_metadata_dir <- fs::path(base_hub_path, "model-metadata")
  fs::dir_create(model_metadata_dir)

  # Create model metadata file with no designated users
  metadata_file <- fs::path(model_metadata_dir, "team1-model.yml")
  writeLines(
    c(
      "team_name: Team 1",
      "model_name: Model 1"
    ),
    metadata_file
  )

  changed_model_ids <- c("team1-model")

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
  # Create a temporary hub directory structure
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_metadata_dir <- fs::path(base_hub_path, "model-metadata")
  fs::dir_create(model_metadata_dir)

  # Create model metadata file for a different model
  metadata_file <- fs::path(model_metadata_dir, "team1-model.yml")
  writeLines(
    c(
      "team_name: Team 1",
      "model_name: Model 1",
      "designated_github_users:",
      "  - user1",
      "  - user2"
    ),
    metadata_file
  )

  changed_model_ids <- c("unknown-model")

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
  # Create a temporary hub directory structure
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_metadata_dir <- fs::path(base_hub_path, "model-metadata")
  fs::dir_create(model_metadata_dir)

  # Create model metadata file
  metadata_file <- fs::path(model_metadata_dir, "team1-model.yml")
  writeLines(
    c(
      "team_name: Team 1",
      "model_name: Model 1",
      "designated_github_users:",
      "  - user1"
    ),
    metadata_file
  )

  changed_model_ids <- c("team1-model")

  expect_message(
    check_authorized_users(
      changed_model_ids = changed_model_ids,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Success: Changes authorized for user 'user1'"
  )
})

test_that("check_authorized_users handles null values in designated_github_users", {
  # Create a temporary hub directory structure
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_metadata_dir <- fs::path(base_hub_path, "model-metadata")
  fs::dir_create(model_metadata_dir)

  # Create model metadata file with null values (which become NA in R)
  metadata_file <- fs::path(model_metadata_dir, "team1-model.yml")
  writeLines(
    c(
      "team_name: Team 1",
      "model_name: Model 1",
      "designated_github_users:",
      "  - user1",
      "  - null",
      "  - user2"
    ),
    metadata_file
  )

  changed_model_ids <- c("team1-model")

  expect_message(
    check_authorized_users(
      changed_model_ids = changed_model_ids,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    ),
    "Success: Changes authorized for user 'user1'"
  )
})
