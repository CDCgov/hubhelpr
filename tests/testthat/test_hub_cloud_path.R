test_that("hub_cloud_path returns the S3 bucket for each supported disease", {
  for (disease in names(hub_s3_buckets)) {
    # endpoint_override making actual network calls in testing.
    path <- hub_cloud_path(disease, endpoint_override = "localhost")

    expect_s3_class(path, "SubTreeFileSystem")
    expect_identical(
      path$base_path,
      paste0(hub_s3_buckets[[disease]], "/")
    )
  }
})

test_that("hub_cloud_path errors on unsupported diseases", {
  expect_error(
    hub_cloud_path("ebola", endpoint_override = "localhost"),
    "covid|rsv|flu"
  )
})
