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

exceptions_path <- function(hub) {
  fs::path(hub, "hub-config", "submission_window_exceptions.toml")
}

test_that("a listed date is an exception", {
  hub <- write_exceptions_hub("dates = [2026-11-12]")
  expect_true(is_submission_window_exception(hub, "2026-11-12"))
})

test_that("an unlisted date is not an exception", {
  hub <- write_exceptions_hub("dates = [2026-11-12]")
  expect_false(is_submission_window_exception(hub, "2026-11-13"))
  expect_false(is_submission_window_exception(hub, "2026-11-11"))
})

test_that("every listed date is excepted, not only the latest", {
  hub <- write_exceptions_hub(c(
    "dates = [",
    "  2026-11-12, # Veterans Day",
    "  2026-11-26, # Thanksgiving",
    "  2026-12-25, # Christmas Day",
    "]"
  ))
  expect_true(is_submission_window_exception(hub, "2026-11-12"))
  expect_true(is_submission_window_exception(hub, "2026-11-26"))
  expect_true(is_submission_window_exception(hub, "2026-12-25"))
  expect_false(is_submission_window_exception(hub, "2026-12-24"))
})

test_that("consecutive days can extend a deadline", {
  hub <- write_exceptions_hub("dates = [2026-11-12, 2026-11-13, 2026-11-14]")
  excepted <- vapply(
    c("2026-11-12", "2026-11-13", "2026-11-14"),
    function(day) is_submission_window_exception(hub, day),
    logical(1)
  )
  expect_true(all(excepted))
  expect_false(is_submission_window_exception(hub, "2026-11-15"))
})

test_that("dates are returned sorted and de-duplicated", {
  hub <- write_exceptions_hub("dates = [2026-12-25, 2026-01-01, 2026-12-25]")
  expect_equal(
    parse_submission_window_exceptions_file(exceptions_path(hub)),
    lubridate::ymd(c("2026-01-01", "2026-12-25"))
  )
})

test_that("a missing exceptions file means no exceptions", {
  hub <- write_exceptions_hub(NULL)
  expect_false(is_submission_window_exception(hub, "2026-11-12"))
  expect_length(
    parse_submission_window_exceptions_file(exceptions_path(hub)),
    0L
  )
})

test_that("an empty dates array means no exceptions", {
  hub <- write_exceptions_hub("dates = []")
  expect_false(is_submission_window_exception(hub, "2026-11-12"))
  expect_length(
    parse_submission_window_exceptions_file(exceptions_path(hub)),
    0L
  )
})

test_that("a file with no dates key means no exceptions", {
  hub <- write_exceptions_hub("# nothing granted yet")
  expect_false(is_submission_window_exception(hub, "2026-11-12"))
})

test_that("comments after dates are ignored", {
  hub <- write_exceptions_hub(c(
    "# standing note",
    "dates = [",
    "  2026-11-12, # Veterans Day on the Wednesday deadline",
    "]"
  ))
  expect_true(is_submission_window_exception(hub, "2026-11-12"))
})

test_that("quoted dates are accepted", {
  hub <- write_exceptions_hub('dates = ["2026-11-12"]')
  expect_true(is_submission_window_exception(hub, "2026-11-12"))
})

test_that("an unparsable date errors", {
  hub <- write_exceptions_hub('dates = ["next thursday"]')
  expect_error(
    parse_submission_window_exceptions_file(exceptions_path(hub)),
    "must be a calendar"
  )
})

test_that("a non-date dates array errors", {
  hub <- write_exceptions_hub("dates = [1, 2, 3]")
  expect_error(
    parse_submission_window_exceptions_file(exceptions_path(hub)),
    "must be TOML dates"
  )
})

test_that("a Date object is accepted as well as a string", {
  hub <- write_exceptions_hub("dates = [2026-11-12]")
  expect_true(
    is_submission_window_exception(hub, lubridate::ymd("2026-11-12"))
  )
})

test_that("the hub timezone is read from admin.json", {
  hub <- write_exceptions_hub(
    "dates = [2026-11-12]",
    timezone = "Pacific/Auckland"
  )
  expect_equal(hub_timezone(hub), "Pacific/Auckland")
})

test_that("an unrecognised timezone warns and falls back to UTC", {
  hub <- write_exceptions_hub("dates = [2026-11-12]", timezone = "Not/AZone")
  expect_warning(
    expect_equal(hub_timezone(hub), "UTC"),
    "not a known timezone"
  )
})

test_that("a hub without admin.json falls back to UTC", {
  hub <- withr::local_tempdir()
  expect_equal(hub_timezone(hub), "UTC")
  expect_false(is_submission_window_exception(hub, "2026-11-12"))
})
