#' S3 bucket storage locations for the supported forecast hubs
#'
#' Maps each disease identifier to the name of the AWS S3 bucket where the
#' corresponding hubverse forecast hub stores its data, as documented in each
#' hub's `hub-config/admin.json` `cloud` configuration.
#'
#' @keywords internal
#' @noRd
hub_s3_buckets <- c(
  covid = "covid19-forecast-hub",
  rsv = "rsv-forecast-hub",
  flu = "cdcepi-flusight-forecast-hub"
)

#' Connect to a forecast hub's cloud (S3) storage
#'
#' Returns a cloud storage path for a supported forecast hub that can be passed
#' to [hubData::connect_hub()] and related hubverse `connect_*` functions in
#' place of a local hub directory path. This avoids having to check out the
#' hub's GitHub repository to read its data.
#'
#' @param disease Disease identifier for the hub. One of `"covid"`, `"rsv"`,
#'   or `"flu"`.
#' @param anonymous Logical. Use anonymous (unauthenticated) access to the
#'   hub's S3 bucket? Passed through to [hubData::s3_bucket()]. The hubverse
#'   buckets are public and interaction from this repository is read-only, so
#'   this defaults to `TRUE`.
#' @param ... Additional keyword arguments passed through to
#'   [hubData::s3_bucket()].
#'
#' @return A `<SubTreeFileSystem>` object pointing at the hub's S3 bucket.
#' @export
#'
#' @examples
#' hub_cloud_path("rsv")
#'
#' hub_cloud_path("covid")$base_path
hub_cloud_path <- function(disease, anonymous = TRUE, ...) {
  disease <- rlang::arg_match0(disease, names(hub_s3_buckets))
  hubData::s3_bucket(hub_s3_buckets[[disease]], anonymous = anonymous, ...)
}
