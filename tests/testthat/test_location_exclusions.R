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

test_that("reference dates absent from the file parse as no exclusions", {
  hub_reports_path <- write_exclusions_file(example_exclusions)
  expect_identical(
    get_reference_date_exclusions_list(hub_reports_path, "covid", "2025-06-07"),
    list()
  )
})

test_that("reference dates with array exclusions parse as applying to every target", {
  hub_reports_path <- write_exclusions_file(example_exclusions)
  expect_identical(
    get_reference_date_exclusions_list(hub_reports_path, "covid", "2025-02-02"),
    list(all = c("AK", "AR"))
  )
})

test_that("reference dates with key value exclusions parse as expected", {
  hub_reports_path <- write_exclusions_file(example_exclusions)
  expect_identical(
    get_reference_date_exclusions_list(hub_reports_path, "covid", "2025-01-01"),
    list("all" = "VI", "wk inc covid hosp" = "GU")
  )
})

test_that("reference dates are looked up identically as Date or character", {
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

test_that("a hub with no exclusions file errors rather than reporting no exclusions", {
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

test_that("each hub resolves to its own exclusions file, not another hub's", {
  hub_reports_path <- write_exclusions_file(example_exclusions, "covid")
  expect_error(
    get_reference_date_exclusions_list(hub_reports_path, "rsv", "2025-02-02"),
    "No exclusions file"
  )
})

test_that("parsed exclusions drop the excluded rows in apply_target_location_exclusions", {
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

test_that("get_reference_date_exclusions_list errors on invalid exclusions files, even if the target reference date has valid entries", {
  hub_reports_path <- write_exclusions_file(c(
    "2025-01-01 = [\"AK\"]",
    "2030-01-01 = [\"ZZ\"]"
  ))

  error <- expect_error(
    get_reference_date_exclusions_list(hub_reports_path, "covid", "2025-01-01")
  )
  expect_match(conditionMessage(error), "invalid abbreviation")
  expect_match(conditionMessage(error), "2030-01-01")
})

test_that("keys that are not YYYY-MM-DD dates are rejected", {
  hub_reports_path <- write_exclusions_file("not_a_date = [\"AK\"]")
  expect_error(
    parse_exclusion_file(exclusion_file_path(hub_reports_path, "covid")),
    "must be a reference date"
  )
})

test_that("a table whose values are not abbreviations is rejected", {
  hub_reports_path <- write_exclusions_file("2025-01-01 = { all = 3 }")
  expect_error(
    parse_exclusion_file(exclusion_file_path(hub_reports_path, "covid")),
    "character"
  )
})

test_that("array and key value exclusions serialize to the JSON the action accepts", {
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

test_that("a lone abbreviation serializes as a JSON array, not a bare string", {
  # auto_unbox would emit a bare string, which the action
  # does not accept
  hub_reports_path <- write_exclusions_file(example_exclusions)
  expect_identical(
    get_reference_date_exclusions_json(hub_reports_path, "covid", "2025-03-03"),
    "{\"all\":[\"VI\"]}"
  )
})

test_that("reference dates absent from the file serialize to the action's default", {
  hub_reports_path <- write_exclusions_file(example_exclusions)
  expect_identical(
    get_reference_date_exclusions_json(hub_reports_path, "covid", "2025-06-07"),
    "[]"
  )
})

test_that("JSON output round-trips back to the parsed exclusions", {
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
