write_minimums_file <- function(contents, disease = "covid") {
  hub_reports_path <- withr::local_tempdir(.local_envir = parent.frame())
  path <- ens_min_designated_models_file_path(hub_reports_path, disease)
  fs::dir_create(fs::path_dir(path))
  writeLines(contents, path)
  hub_reports_path
}

example_minimums <- c(
  "# the rule was introduced for 2026-04-04",
  "2024-11-23 = 0",
  "2026-04-04 = 2"
)

test_that("reference dates take the minimum from the latest entry at or before them", {
  hub_reports_path <- write_minimums_file(example_minimums)

  expect_identical(
    get_reference_date_ens_minimum(hub_reports_path, "covid", "2024-11-23"),
    0L
  )
  expect_identical(
    get_reference_date_ens_minimum(hub_reports_path, "covid", "2026-03-28"),
    0L
  )
  expect_identical(
    get_reference_date_ens_minimum(hub_reports_path, "covid", "2026-04-04"),
    2L
  )
})

test_that("reference dates after the last entry keep the latest minimum", {
  # the last entry is open; thus future weeks need no
  # maintenance unless the rule changes
  hub_reports_path <- write_minimums_file(example_minimums)

  expect_identical(
    get_reference_date_ens_minimum(hub_reports_path, "covid", "2030-06-01"),
    2L
  )
})

test_that("a one-off minimum applies to its week and no others", {
  # the way to change the minimum for a single week is
  # to add an entry for it and another restoring the
  # previous value for the week after
  hub_reports_path <- write_minimums_file(c(
    "2024-11-23 = 0",
    "2026-04-04 = 2",
    "2026-05-02 = 3",
    "2026-05-09 = 2"
  ))

  expect_identical(
    get_reference_date_ens_minimum(hub_reports_path, "covid", "2026-04-25"),
    2L
  )
  expect_identical(
    get_reference_date_ens_minimum(hub_reports_path, "covid", "2026-05-02"),
    3L
  )
  expect_identical(
    get_reference_date_ens_minimum(hub_reports_path, "covid", "2026-05-09"),
    2L
  )
})

test_that("reference dates are looked up identically as Date or character", {
  hub_reports_path <- write_minimums_file(example_minimums)

  expect_identical(
    get_reference_date_ens_minimum(
      hub_reports_path,
      "covid",
      as.Date("2026-04-04")
    ),
    get_reference_date_ens_minimum(hub_reports_path, "covid", "2026-04-04")
  )
})

test_that("entries out of order in the file are still resolved by date", {
  hub_reports_path <- write_minimums_file(c(
    "2026-04-04 = 2",
    "2024-11-23 = 0"
  ))

  expect_identical(
    get_reference_date_ens_minimum(hub_reports_path, "covid", "2026-03-28"),
    0L
  )
})

test_that("a reference date before the record begins is an error", {
  hub_reports_path <- write_minimums_file(example_minimums)

  expect_error(
    get_reference_date_ens_minimum(hub_reports_path, "covid", "2020-01-04"),
    "No ensemble reporting minimum recorded"
  )
})

test_that("a hub with no minimums file errors rather than assuming a default", {
  hub_reports_path <- withr::local_tempdir()

  expect_error(
    get_reference_date_ens_minimum(hub_reports_path, "covid", "2026-04-04"),
    "No ensemble reporting minimums file"
  )
})

test_that("each hub resolves to its own minimums file, not another hub's", {
  hub_reports_path <- write_minimums_file(example_minimums, "covid")

  expect_error(
    get_reference_date_ens_minimum(hub_reports_path, "rsv", "2026-04-04"),
    "No ensemble reporting minimums file"
  )
})

test_that("keys that are not YYYY-MM-DD dates are rejected", {
  hub_reports_path <- write_minimums_file("not_a_date = 2")

  expect_error(
    parse_ens_min_designated_models_file(ens_min_designated_models_file_path(
      hub_reports_path,
      "covid"
    )),
    "must be a reference date"
  )
})

test_that("minimums that are not counts are rejected", {
  not_counts <- c(
    negative = "2026-04-04 = -1",
    fractional = "2026-04-04 = 2.5",
    string = "2026-04-04 = \"two\"",
    boolean = "2026-04-04 = true"
  )

  purrr::iwalk(not_counts, \(entry, label) {
    hub_reports_path <- write_minimums_file(entry)
    expect_error(
      parse_ens_min_designated_models_file(ens_min_designated_models_file_path(
        hub_reports_path,
        "covid"
      )),
      "minimums in",
      info = label
    )
  })
})
