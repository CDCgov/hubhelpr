population_data <- readr::read_csv(
  fs::path("inst", "extdata", "locations_with_2023_census_pop.csv"),
  show_col_types = FALSE
) |>
  dplyr::select("location", "population")

usethis::use_data(population_data, overwrite = TRUE)
