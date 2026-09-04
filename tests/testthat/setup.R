library(httptest2)
mockdir_tests <- fs::path("api_mocks")

example_hub_paths <- purrr::pmap_vec(
  tidyr::crossing(
    version = c("v5", "v6"),
    type = c("target_dir", "target_file")
  ),
  \(version, type) {
    system.file(
      fs::path("testhubs", version, type),
      package = "hubUtils"
    )
  }
)

example_cfa_hub <- system.file(
  fs::path("testhubs", "covidhub"),
  package = "hubhelpr"
)


## replace env variables with fakes if and only if
## we are mocking api calls
if (fs::dir_exists(mockdir_tests)) {
  withr::local_envvar(
    .new = c(
      "DATA_CDC_GOV_API_KEY_ID" = "fake_key",
      "DATA_CDC_GOV_API_KEY_SECRET" = "fake_secret" #pragma: allowlist secret
    )
  )
}
