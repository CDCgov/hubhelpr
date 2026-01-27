# Tests for check_changes_for_autoapproval
#
# This test file validates the check_changes_for_autoapproval function,
# which is used in GitHub workflows to validate that PR changes are eligible
# for auto-approval.
#
# Testing strategy:
# - Tests that don't require authorization checking can run without mocking
# - Tests that require authorization checking use mockery::stub to mock
#   hubData::load_model_metadata, avoiding the need for complex hub fixtures
# - Tests validate both success cases (authorized changes) and error cases
#   (unauthorized changes, files outside model-output, etc.)

test_that("check_changes_for_autoapproval succeeds with valid changes to model-output", {
  skip_if_not_installed("mockery")
  
  # Create a temporary hub directory structure
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_output_dir <- fs::path(base_hub_path, "model-output", "team1-model")
  fs::dir_create(model_output_dir)
  
  # Create a test file in model-output
  test_file <- fs::path(model_output_dir, "test.csv")
  writeLines("test", test_file)
  
  changed_files <- c(as.character(test_file))
  
  # Mock hubData::load_model_metadata to return expected structure
  mockery::stub(
    check_changes_for_autoapproval,
    "hubData::load_model_metadata",
    tibble::tibble(
      model_id = "team1-model",
      designated_github_users = list(c("user1", "user2"))
    )
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
  
  # Create a file outside model-output
  other_dir <- fs::path(base_hub_path, "other-dir")
  fs::dir_create(other_dir)
  test_file <- fs::path(other_dir, "test.txt")
  writeLines("test", test_file)
  
  changed_files <- c(as.character(test_file))
  
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
  base_hub_path <- withr::local_tempdir("test_hub_")
  
  # Create a file inside model-output
  model_output_dir <- fs::path(base_hub_path, "model-output", "team1-model")
  fs::dir_create(model_output_dir)
  model_file <- fs::path(model_output_dir, "test.csv")
  writeLines("test", model_file)
  
  # Create a file outside model-output
  other_dir <- fs::path(base_hub_path, "other-dir")
  fs::dir_create(other_dir)
  other_file <- fs::path(other_dir, "test.txt")
  writeLines("test", other_file)
  
  changed_files <- c(as.character(model_file), as.character(other_file))
  
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
  skip_if_not_installed("mockery")
  
  base_hub_path <- withr::local_tempdir("test_hub_")
  model_output_dir <- fs::path(base_hub_path, "model-output", "team1-model")
  fs::dir_create(model_output_dir)
  
  test_file <- fs::path(model_output_dir, "test.csv")
  writeLines("test", test_file)
  
  changed_files <- c(as.character(test_file))
  
  # Mock hubData::load_model_metadata to return model with different authorized users
  mockery::stub(
    check_changes_for_autoapproval,
    "hubData::load_model_metadata",
    tibble::tibble(
      model_id = "team1-model",
      designated_github_users = list(c("other-user1", "other-user2"))
    )
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
  skip_if_not_installed("mockery")
  
  base_hub_path <- withr::local_tempdir("test_hub_")
  
  # Create two model directories
  model1_dir <- fs::path(base_hub_path, "model-output", "team1-model")
  model2_dir <- fs::path(base_hub_path, "model-output", "team2-model")
  fs::dir_create(model1_dir)
  fs::dir_create(model2_dir)
  
  # Create files in both models
  file1 <- fs::path(model1_dir, "test1.csv")
  file2 <- fs::path(model2_dir, "test2.csv")
  writeLines("test", file1)
  writeLines("test", file2)
  
  changed_files <- c(as.character(file1), as.character(file2))
  
  # Mock hubData::load_model_metadata
  # user1 is authorized for team1-model but not team2-model
  mockery::stub(
    check_changes_for_autoapproval,
    "hubData::load_model_metadata",
    tibble::tibble(
      model_id = c("team1-model", "team2-model"),
      designated_github_users = list(
        c("user1", "user2"),
        c("other-user")
      )
    )
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
  skip_if_not_installed("mockery")
  
  base_hub_path <- withr::local_tempdir("test_hub_")
  
  # Create two model directories
  model1_dir <- fs::path(base_hub_path, "model-output", "team1-model")
  model2_dir <- fs::path(base_hub_path, "model-output", "team2-model")
  fs::dir_create(model1_dir)
  fs::dir_create(model2_dir)
  
  # Create files in both models
  file1 <- fs::path(model1_dir, "test1.csv")
  file2 <- fs::path(model2_dir, "test2.csv")
  writeLines("test", file1)
  writeLines("test", file2)
  
  changed_files <- c(as.character(file1), as.character(file2))
  
  # Mock hubData::load_model_metadata
  # user1 is authorized for both models
  mockery::stub(
    check_changes_for_autoapproval,
    "hubData::load_model_metadata",
    tibble::tibble(
      model_id = c("team1-model", "team2-model"),
      designated_github_users = list(
        c("user1", "user2"),
        c("user1", "other-user")
      )
    )
  )
  
  expect_silent(
    check_changes_for_autoapproval(
      changed_files = changed_files,
      gh_actor = "user1",
      base_hub_path = base_hub_path
    )
  )
})
