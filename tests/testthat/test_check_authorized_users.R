test_that("check_authorized_users succeeds when user authorized for all models", {
  skip_if_not_installed("mockery")
  
  base_hub_path <- withr::local_tempdir("test_hub_")
  
  changed_model_ids <- c("team1-model", "team2-model")
  
  # Mock hubData::load_model_metadata
  mockery::stub(
    check_authorized_users,
    "hubData::load_model_metadata",
    tibble::tibble(
      model_id = c("team1-model", "team2-model"),
      designated_github_users = list(
        c("user1", "user2"),
        c("user1", "user3")
      )
    )
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
  skip_if_not_installed("mockery")
  
  base_hub_path <- withr::local_tempdir("test_hub_")
  
  changed_model_ids <- c("team1-model")
  
  # Mock hubData::load_model_metadata
  mockery::stub(
    check_authorized_users,
    "hubData::load_model_metadata",
    tibble::tibble(
      model_id = "team1-model",
      designated_github_users = list(c("user1", "user2"))
    )
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
  skip_if_not_installed("mockery")
  
  base_hub_path <- withr::local_tempdir("test_hub_")
  
  changed_model_ids <- c("team1-model", "team2-model")
  
  # Mock hubData::load_model_metadata
  # user1 authorized for team1-model but not team2-model
  mockery::stub(
    check_authorized_users,
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
  skip_if_not_installed("mockery")
  
  base_hub_path <- withr::local_tempdir("test_hub_")
  
  changed_model_ids <- c("team1-model")
  
  # Mock hubData::load_model_metadata with no designated users
  mockery::stub(
    check_authorized_users,
    "hubData::load_model_metadata",
    tibble::tibble(
      model_id = "team1-model",
      designated_github_users = list(character(0))
    )
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
  skip_if_not_installed("mockery")
  
  base_hub_path <- withr::local_tempdir("test_hub_")
  
  changed_model_ids <- c("unknown-model")
  
  # Mock hubData::load_model_metadata with different models
  mockery::stub(
    check_authorized_users,
    "hubData::load_model_metadata",
    tibble::tibble(
      model_id = "team1-model",
      designated_github_users = list(c("user1", "user2"))
    )
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
  skip_if_not_installed("mockery")
  
  base_hub_path <- withr::local_tempdir("test_hub_")
  
  changed_model_ids <- c("team1-model")
  
  # Mock hubData::load_model_metadata
  mockery::stub(
    check_authorized_users,
    "hubData::load_model_metadata",
    tibble::tibble(
      model_id = "team1-model",
      designated_github_users = list(c("user1"))
    )
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
  skip_if_not_installed("mockery")
  
  base_hub_path <- withr::local_tempdir("test_hub_")
  
  changed_model_ids <- c("team1-model")
  
  # Mock hubData::load_model_metadata with NA values
  mockery::stub(
    check_authorized_users,
    "hubData::load_model_metadata",
    tibble::tibble(
      model_id = "team1-model",
      designated_github_users = list(c("user1", NA_character_, "user2"))
    )
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
