# Writes newline-separated contact lists of model-contributor email addresses
# for the RSV and COVID-19 forecast hubs using sparse hub clones.
#
# Writes rsv_contacts.txt and covid_contacts.txt into the current directory.
# The first argument can be used to write them elsewhere, e.g.
#
#   Rscript make_hub_contact_lists.R ../
#
# N.B.: addresses are lowercased, de-duplicated, and sorted.

library(cli)
library(hubData)
library(purrr)

HUB_URLS <- c(
  rsv = "https://github.com/CDCgov/rsv-forecast-hub",
  covid = "https://github.com/CDCgov/covid19-forecast-hub"
)

HUB_SPARSE_PATHS <- c("model-metadata", "hub-config")

#' Clone hub repository metadata.
#'
#' @param hub_url character, URL of a hub repository.
#' @param hub_path character, directory to clone into.
#' @return character, `hub_path`, invisibly.
clone_hub_metadata <- function(hub_url, hub_path) {
  git <- function(...) {
    args <- c(...)
    if (system2("git", args, stdout = FALSE, stderr = FALSE) != 0L) {
      cli::cli_abort("{.code git {paste(args, collapse = ' ')}} failed.")
    }
  }
  git(
    "clone",
    "--depth",
    "1",
    "--filter=blob:none",
    "--sparse",
    hub_url,
    hub_path
  )
  git("-C", hub_path, "sparse-checkout", "set", HUB_SPARSE_PATHS)
  invisible(hub_path)
}

#' Pull contributor email addresses out of a hub's model metadata.
#'
#' @param hub_path character, path to a hub root containing `model-metadata/`.
#' @return character vector of addresses, lowercased, de-duplicated, and
#' sorted.
hub_contact_emails <- function(hub_path) {
  metadata <- suppressWarnings(hubData::load_model_metadata(hub_path))
  metadata$model_contributors |>
    unlist(recursive = FALSE) |>
    purrr::map_chr("email", .default = NA_character_) |>
    trimws() |>
    tolower() |>
    purrr::discard(is.na) |>
    purrr::keep(nzchar) |>
    unique() |>
    sort()
}

#' Write a contact list for every hub in `HUB_URLS`.
#'
#' @param output_dir character, directory to write the lists into.
#' @return named list of address vectors, invisibly.
make_hub_contact_lists <- function(output_dir = ".") {
  lists <- lapply(names(HUB_URLS), function(hub) {
    hub_path <- file.path(tempdir(), hub)
    on.exit(unlink(hub_path, recursive = TRUE), add = TRUE)
    clone_hub_metadata(HUB_URLS[[hub]], hub_path)
    emails <- hub_contact_emails(hub_path)
    output_path <- file.path(output_dir, paste0(hub, "_contacts.txt"))
    writeLines(emails, output_path)
    cli::cli_alert_success(
      "{hub}: {length(emails)} address{?es} in {.file {normalizePath(output_path)}}"
    )
    emails
  })
  names(lists) <- names(HUB_URLS)
  invisible(lists)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  make_hub_contact_lists(output_dir = if (length(args) >= 1) args[[1]] else ".")
}
