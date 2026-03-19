## example_hub_paths defined in testthat/setup.R

.test_hub_supported_targets_against_oracle <- function(hub) {
  test_that(
    glue::glue(
      "get_hub_supported_targets() output is ",
      "with the set of unique targets in the ",
      "oracle output for test hub {hub}"
    ),
    {
      expected <- hubData::connect_target_oracle_output(hub) |>
        dplyr::distinct(.data$target) |>
        dplyr::collect() |>
        dplyr::pull()

      checkmate::expect_names(
        get_hub_supported_targets(hub),
        permutation.of = expected
      )
    }
  )
}

purrr::walk(example_hub_paths, .test_hub_supported_targets_against_oracle)
