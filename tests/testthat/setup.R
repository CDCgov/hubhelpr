library(httptest2)
mockdir <- "api_mocks"

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
