test_that("bundled covidhub test hub includes required custom metadata cases", {
  hub_path <- fs::path("inst", "testhubs", "covidhub")
  meta_path <- fs::path(hub_path, "model-metadata")

  expect_true(fs::dir_exists(hub_path))
  expect_true(fs::dir_exists(meta_path))

  baseline <- paste(
    readLines(fs::path(meta_path, "CovidHub-baseline.yaml")),
    collapse = "\n"
  )
  epiautogp <- paste(
    readLines(fs::path(meta_path, "CFA-EpiAutoGP.yaml")),
    collapse = "\n"
  )
  pyrenew_hew <- paste(
    readLines(fs::path(meta_path, "CFA_Pyrenew-PyrenewHEW_COVID.yaml")),
    collapse = "\n"
  )
  pyrenew_e <- paste(
    readLines(fs::path(meta_path, "CFA_Pyrenew-Pyrenew_E_COVID.yaml")),
    collapse = "\n"
  )
  pyrenew_he <- paste(
    readLines(fs::path(meta_path, "CFA_Pyrenew-Pyrenew_HE_COVID.yaml")),
    collapse = "\n"
  )
  ceph_rtrend <- paste(
    readLines(fs::path(meta_path, "CEPH-Rtrend_covid.yml")),
    collapse = "\n"
  )

  # designated_github_users: absent, length 1, length > 1
  expect_false(grepl("designated_github_users", baseline, fixed = TRUE))
  expect_true(
    grepl(
      "designated_github_users: \\[\"SamuelBrand1\"\\]",
      epiautogp
    )
  )
  expect_true(
    grepl(
      "designated_github_users: \\[\"dylanhmorris\", \"damonbayer\", \"sbidari\", \"O957\"\\]",
      pyrenew_hew
    )
  )

  # designated_targets for designated_model: false
  expect_true(grepl("designated_model: false", baseline, fixed = TRUE))
  expect_false(grepl("designated_targets", baseline, fixed = TRUE))
  expect_true(
    grepl(
      "designated_targets: \\[\"wk inc covid prop ed visits\"\\]",
      pyrenew_e
    )
  )
  expect_true(
    grepl(
      "designated_targets: \\[\"wk inc covid hosp\", \"wk inc covid prop ed visits\"\\]",
      pyrenew_hew
    )
  )

  # designated_targets for designated_model: true
  expect_true(grepl("designated_model: true", ceph_rtrend, fixed = TRUE))
  expect_false(grepl("designated_targets", ceph_rtrend, fixed = TRUE))
  expect_true(
    grepl(
      "designated_targets: \\[\"wk inc covid hosp\"\\]",
      epiautogp
    )
  )
  expect_true(
    grepl(
      "designated_targets: \\[\"wk inc covid hosp\", \"wk inc covid prop ed visits\"\\]",
      pyrenew_he
    )
  )
})
