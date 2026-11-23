write_exceptions_hub <- function(lines, timezone = "America/New_York") {
  hub <- withr::local_tempdir(.local_envir = parent.frame())
  fs::dir_create(fs::path(hub, "hub-config"))
  jsonlite::write_json(
    list(timezone = timezone),
    fs::path(hub, "hub-config", "admin.json"),
    auto_unbox = TRUE
  )
  if (!is.null(lines)) {
    writeLines(
      lines,
      fs::path(hub, "hub-config", "submission_window_exceptions.toml")
    )
  }
  return(hub)
}

test_that("a recorded date is an exception", {
  hub <- write_exceptions_hub(
    '2026-11-12 = "NHSN publication delayed past the Wednesday deadline"'
  )
  expect_true(is_submission_window_exception(hub, "2026-11-12"))
})

test_that("an unrecorded date is not an exception", {
  hub <- write_exceptions_hub(
    '2026-11-12 = "NHSN publication delayed past the Wednesday deadline"'
  )
  expect_false(is_submission_window_exception(hub, "2026-11-13"))
  expect_false(is_submission_window_exception(hub, "2026-11-11"))
})

test_that("a missing exceptions file means no exceptions", {
  hub <- write_exceptions_hub(NULL)
  expect_false(is_submission_window_exception(hub, "2026-11-12"))
  expect_true(is.na(parse_submission_window_exceptions_file(
    fs::path(hub, "hub-config", "submission_window_exceptions.toml")
  )))
})

test_that("an empty exceptions file means no exceptions", {
  hub <- write_exceptions_hub(character())
  expect_false(is_submission_window_exception(hub, "2026-11-12"))
})

test_that("only the latest of several exceptions is returned", {
  hub <- write_exceptions_hub(c(
    '2026-11-12 = "second"',
    '2026-01-08 = "first"'
  ))
  parsed <- parse_submission_window_exceptions_file(
    fs::path(hub, "hub-config", "submission_window_exceptions.toml")
  )
  expect_length(parsed, 1L)
  expect_equal(parsed, lubridate::ymd("2026-11-12"))
})

test_that("an earlier exception no longer matches once a later one is added", {
  hub <- write_exceptions_hub(c(
    '2026-01-08 = "first"',
    '2026-11-12 = "second"'
  ))
  expect_true(is_submission_window_exception(hub, "2026-11-12"))
  expect_false(is_submission_window_exception(hub, "2026-01-08"))
})

test_that("a malformed date key errors", {
  hub <- write_exceptions_hub('"next thursday" = "not a date"')
  expect_error(
    parse_submission_window_exceptions_file(
      fs::path(hub, "hub-config", "submission_window_exceptions.toml")
    ),
    "must be a calendar date"
  )
})

test_that("an empty reason errors", {
  hub <- write_exceptions_hub('2026-11-12 = ""')
  expect_error(
    parse_submission_window_exceptions_file(
      fs::path(hub, "hub-config", "submission_window_exceptions.toml")
    )
  )
})

test_that("a Date object is accepted as well as a string", {
  hub <- write_exceptions_hub('2026-11-12 = "delayed data"')
  expect_true(
    is_submission_window_exception(hub, lubridate::ymd("2026-11-12"))
  )
})

test_that("the hub timezone is read from admin.json", {
  hub <- write_exceptions_hub(
    '2026-11-12 = "delayed data"',
    timezone = "Pacific/Auckland"
  )
  expect_equal(hub_timezone(hub), "Pacific/Auckland")
})

test_that("a hub without admin.json falls back to UTC", {
  hub <- withr::local_tempdir()
  expect_equal(hub_timezone(hub), "UTC")
  expect_false(is_submission_window_exception(hub, "2026-11-12"))
})
