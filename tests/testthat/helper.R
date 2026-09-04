.example_ens_minimums_content <- c(
  "# the rule was introduced for 2026-04-04",
  "2024-11-23 = 0",
  "2026-04-04 = 2"
)

#' Write a mock record of ensemble minima
#' to the given directory (for dynamically created
#' hub reports directories in testing
write_mock_ens_minimums_file <- function(
  reports_dir,
  disease,
  content = .example_ens_minimums_content
) {
  path <- ens_min_designated_models_file_path(reports_dir, disease)
  fs::dir_create(fs::path_dir(path))
  writeLines(content, path)
}

#' Wrapper of withr::local_envvar to
#' replace env variables with fakes if and only if
#' we are mocking api calls
replace_env_vars_if_mocking <- function(
  mockdir,
  .local_envir = parent.frame()
) {
  if (fs::dir_exists(mockdir)) {
    withr::local_envvar(
      .new = c(
        "DATA_CDC_GOV_API_KEY_ID" = "fake_key",
        "DATA_CDC_GOV_API_KEY_SECRET" = "fake_secret" #pragma: allowlist secret
      ),
      .local_envir = .local_envir
    )
  }
}
