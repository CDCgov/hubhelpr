test_that("bundled covidhub test hub includes required custom metadata cases", {
  hub_path <- system.file("testhubs", "covidhub", package = "hubhelpr")

  expect_true(nzchar(hub_path))
  expect_true(fs::dir_exists(hub_path))
  all_metadata <- hubData::load_model_metadata(hub_path)

  get_model_values <- function(model_id, col_name) {
    values <- all_metadata[
      all_metadata$model_id == model_id,
      col_name,
      drop = TRUE
    ] |>
      unique()

    values[!is.na(values)]
  }

  get_designated_model <- function(model_id) {
    values <- all_metadata[
      all_metadata$model_id == model_id,
      "designated_model",
      drop = TRUE
    ] |>
      unique()

    values[!is.na(values)]
  }

  # designated_github_users: absent, length 1, length > 1
  expect_length(get_model_values("CovidHub-baseline", "designated_github_users"), 0)
  expect_equal(
    get_model_values("CFA-EpiAutoGP", "designated_github_users"),
    "SamuelBrand1"
  )
  expect_equal(
    sort(get_model_values("CFA_Pyrenew-PyrenewHEW_COVID", "designated_github_users")),
    sort(c("dylanhmorris", "damonbayer", "sbidari", "O957"))
  )

  # designated_targets for designated_model: false
  expect_identical(get_designated_model("CovidHub-baseline"), FALSE)
  expect_length(get_model_values("CovidHub-baseline", "designated_targets"), 0)
  expect_identical(get_designated_model("CFA_Pyrenew-Pyrenew_E_COVID"), FALSE)
  expect_equal(
    get_model_values("CFA_Pyrenew-Pyrenew_E_COVID", "designated_targets"),
    "wk inc covid prop ed visits"
  )
  expect_identical(get_designated_model("CovidHub-ensemble"), FALSE)
  expect_equal(
    sort(get_model_values("CovidHub-ensemble", "designated_targets")),
    sort(c("wk inc covid hosp", "wk inc covid prop ed visits"))
  )

  # designated_targets for designated_model: true
  expect_identical(get_designated_model("CFA_Pyrenew-PyrenewHEW_COVID"), TRUE)
  expect_length(get_model_values("CFA_Pyrenew-PyrenewHEW_COVID", "designated_targets"), 0)
  expect_identical(get_designated_model("CFA-EpiAutoGP"), TRUE)
  expect_equal(
    get_model_values("CFA-EpiAutoGP", "designated_targets"),
    "wk inc covid hosp"
  )
  expect_identical(get_designated_model("CFA_Pyrenew-Pyrenew_HE_COVID"), TRUE)
  expect_equal(
    sort(get_model_values("CFA_Pyrenew-Pyrenew_HE_COVID", "designated_targets")),
    sort(c("wk inc covid hosp", "wk inc covid prop ed visits"))
  )
})
