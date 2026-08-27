write_exclusions_file <- function(contents, disease = "covid") {
  hub_reports_path <- withr::local_tempdir(.local_envir = parent.frame())
  path <- exclusion_file_path(hub_reports_path, disease)
  fs::dir_create(fs::path_dir(path))
  writeLines(contents, path)
  hub_reports_path
}

example_exclusions <- c(
  "# DC reported no ED visit data this week",
  "2026-02-28 = { \"wk inc covid prop ed visits\" = [\"DC\"] }",
  "2025-01-01 = { all = [\"VI\"], \"wk inc covid hosp\" = [\"GU\"] }",
  "2025-02-02 = [\"AK\", \"AR\"]",
  "2025-03-03 = [\"VI\"]"
)

test_that("a date with no entry has no exclusions", {
  hub_reports_path <- write_exclusions_file(example_exclusions)
  expect_identical(
    get_reference_date_exclusions_list(hub_reports_path, "covid", "2025-06-07"),
    list()
  )
})

test_that("an array applies to every target", {
  hub_reports_path <- write_exclusions_file(example_exclusions)
  expect_identical(
    get_reference_date_exclusions_list(hub_reports_path, "covid", "2025-02-02"),
    list(all = c("AK", "AR"))
  )
})

test_that("a table maps targets to abbreviations", {
  hub_reports_path <- write_exclusions_file(example_exclusions)
  expect_identical(
    get_reference_date_exclusions_list(hub_reports_path, "covid", "2025-01-01"),
    list("all" = "VI", "wk inc covid hosp" = "GU")
  )
})

test_that("reference dates are accepted as dates or strings", {
  hub_reports_path <- write_exclusions_file(example_exclusions)
  expect_identical(
    get_reference_date_exclusions_list(
      hub_reports_path,
      "covid",
      as.Date("2025-02-02")
    ),
    get_reference_date_exclusions_list(hub_reports_path, "covid", "2025-02-02")
  )
})

test_that("a missing file is an error, not an empty policy", {
  # absence is indistinguishable from a wrong path, and silently
  # applying no exclusions is the failure this file prevents
  hub_reports_path <- withr::local_tempdir()
  expect_error(
    parse_exclusion_file(exclusion_file_path(hub_reports_path, "covid")),
    "No exclusions file"
  )
  expect_error(
    get_reference_date_exclusions_list(hub_reports_path, "covid", "2025-01-01"),
    "No exclusions file"
  )
})

test_that("each hub reads its own file", {
  hub_reports_path <- write_exclusions_file(example_exclusions, "covid")
  expect_error(
    get_reference_date_exclusions_list(hub_reports_path, "rsv", "2025-02-02"),
    "No exclusions file"
  )
})

test_that("an entry can be passed straight to apply_target_location_exclusions", {
  hub_reports_path <- write_exclusions_file(example_exclusions)

  forecasts <- tibble::tibble(
    target = "wk inc covid hosp",
    location = forecasttools::us_location_recode(
      c("AK", "AR", "CA"),
      "abbr",
      "hub"
    )
  )

  kept <- apply_target_location_exclusions(
    forecasts,
    get_reference_date_exclusions_list(hub_reports_path, "covid", "2025-02-02"),
    example_cfa_hub
  )

  expect_identical(
    kept$location,
    forecasttools::us_location_recode("CA", "abbr", "hub")
  )
})

test_that("errors name the week that is wrong", {
  hub_reports_path <- write_exclusions_file(c(
    "2025-01-01 = [\"AK\"]",
    "2030-01-01 = [\"ZZ\"]"
  ))
  expect_error(
    parse_exclusion_file(exclusion_file_path(hub_reports_path, "covid")),
    "2030-01-01"
  )
})

test_that("invalid abbreviations are caught on read, not on use", {
  hub_reports_path <- write_exclusions_file(c(
    "2025-01-01 = [\"AK\"]",
    "2030-01-01 = [\"ZZ\"]"
  ))
  expect_error(
    get_reference_date_exclusions_list(hub_reports_path, "covid", "2025-01-01"),
    "invalid abbreviation"
  )
})

test_that("keys that are not reference dates are rejected", {
  hub_reports_path <- write_exclusions_file("not_a_date = [\"AK\"]")
  expect_error(
    parse_exclusion_file(exclusion_file_path(hub_reports_path, "covid")),
    "must be a reference date"
  )
})

test_that("an unnamed table is rejected", {
  hub_reports_path <- write_exclusions_file("2025-01-01 = { all = 3 }")
  expect_error(
    parse_exclusion_file(exclusion_file_path(hub_reports_path, "covid")),
    "character"
  )
})

test_that("JSON output matches what the action accepts", {
  hub_reports_path <- write_exclusions_file(example_exclusions)

  expect_identical(
    get_reference_date_exclusions_json(hub_reports_path, "covid", "2025-02-02"),
    "{\"all\":[\"AK\",\"AR\"]}"
  )
  expect_identical(
    get_reference_date_exclusions_json(hub_reports_path, "covid", "2025-01-01"),
    "{\"all\":[\"VI\"],\"wk inc covid hosp\":[\"GU\"]}"
  )
})

test_that("a single abbreviation stays an array in JSON", {
  # auto_unbox would emit a bare string, which the action
  # does not accept
  hub_reports_path <- write_exclusions_file(example_exclusions)
  expect_identical(
    get_reference_date_exclusions_json(hub_reports_path, "covid", "2025-03-03"),
    "{\"all\":[\"VI\"]}"
  )
})

test_that("a date with no entry emits the action's own default", {
  hub_reports_path <- write_exclusions_file(example_exclusions)
  expect_identical(
    get_reference_date_exclusions_json(hub_reports_path, "covid", "2025-06-07"),
    "[]"
  )
})

test_that("JSON output parses back to the original entry", {
  hub_reports_path <- write_exclusions_file(example_exclusions)
  purrr::walk(c("2025-01-01", "2025-02-02", "2025-03-03"), \(reference_date) {
    json <- get_reference_date_exclusions_json(
      hub_reports_path,
      "covid",
      reference_date
    )
    expect_identical(
      jsonlite::fromJSON(json),
      get_reference_date_exclusions_list(
        hub_reports_path,
        "covid",
        reference_date
      ),
      info = reference_date
    )
  })
})

test_that("comments are preserved as a format affordance", {
  hub_reports_path <- write_exclusions_file(example_exclusions)
  expect_identical(
    get_reference_date_exclusions_list(hub_reports_path, "covid", "2026-02-28"),
    list("wk inc covid prop ed visits" = "DC")
  )
})
