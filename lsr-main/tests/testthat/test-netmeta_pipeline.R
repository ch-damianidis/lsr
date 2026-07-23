library(testthat)
source("../../utils/netmeta_pipeline.R")
source("../../utils/data_processing.R")

test_that("build_nm validates required columns", {
  invalid_df <- data.frame(
    study  = c("Study1"),
    treat1 = c("A"),
    treat2 = c("B"),
    logHR  = c(0.1),
    stringsAsFactors = FALSE
  )
  expect_error(build_nm(invalid_df), "Missing columns: selogHR")
})

test_that("build_nm accepts valid pairwise data", {
  skip_if_not_installed("netmeta")
  valid_df <- data.frame(
    study   = c("Study1", "Study2", "Study3"),
    treat1  = c("A", "A", "B"),
    treat2  = c("B", "C", "C"),
    logHR   = c(0.1, 0.2, 0.3),
    selogHR = c(0.05, 0.05, 0.05),
    stringsAsFactors = FALSE
  )
  res <- build_nm(valid_df)
  expect_s3_class(res, "netmeta")
})

test_that("run_cnma_analysis returns netcomb for connected network", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("igraph")
  connected_df <- data.frame(
    study   = c("Study1", "Study2", "Study3"),
    treat1  = c("A", "B", "A"),
    treat2  = c("B", "C", "C"),
    logHR   = c(0.1, 0.2, 0.3),
    selogHR = c(0.05, 0.05, 0.05),
    stringsAsFactors = FALSE
  )
  res <- run_cnma_analysis(connected_df)
  expect_s3_class(res, "netcomb")
})

test_that("run_cnma_analysis returns discomb for disconnected network", {
  skip_if_not_installed("netmeta")
  skip_if_not_installed("igraph")
  disconnected_df <- data.frame(
    study   = c("Study1", "Study2"),
    treat1  = c("A", "C"),
    treat2  = c("B", "D"),
    logHR   = c(0.1, 0.2),
    selogHR = c(0.05, 0.05),
    stringsAsFactors = FALSE
  )
  res <- run_cnma_analysis(disconnected_df)
  expect_s3_class(res, "discomb")
})
