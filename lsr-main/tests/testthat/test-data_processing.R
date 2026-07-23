library(testthat)
source("../../utils/data_processing.R")

test_that("summarize_data works with pairwise data (3 rows, 2 studies, 3 treatments)", {
  df_pw <- data.frame(
    study   = c("Study1", "Study1", "Study2"),
    treat1  = c("A", "A", "B"),
    treat2  = c("B", "C", "C"),
    logHR   = c(0.1, 0.2, 0.3),
    selogHR = c(0.05, 0.05, 0.05),
    stringsAsFactors = FALSE
  )
  res <- summarize_data(df_pw)
  expect_equal(res$n_studies, 2)
  expect_equal(res$n_arms, 3)
  expect_equal(res$n_treatments, 3)
})

test_that("summarize_data works with arm-level data", {
  df_arm <- data.frame(
    study     = c("Study1", "Study1", "Study2", "Study2"),
    treatment = c("A", "B", "A", "B"),
    mean      = c(10, 12, 11, 13),
    sd        = c(2, 2, 2, 2),
    n         = c(50, 50, 50, 50),
    stringsAsFactors = FALSE
  )
  res <- summarize_data(df_arm)
  expect_equal(res$n_studies, 2)
  expect_equal(res$n_arms, 4)
  expect_equal(res$n_treatments, 2)
})

test_that("convert_to_pairwise returns unchanged if already pairwise", {
  df_pw <- data.frame(
    study   = c("Study1", "Study2"),
    treat1  = c("A", "B"),
    treat2  = c("B", "C"),
    logHR   = c(0.1, 0.2),
    selogHR = c(0.05, 0.05),
    stringsAsFactors = FALSE
  )
  res <- convert_to_pairwise(df_pw)
  expect_equal(res, df_pw)
})

test_that("summarize_data reports missing data correctly", {
  df_miss <- data.frame(
    study   = c("Study1", "Study2"),
    treat1  = c("A", NA),
    treat2  = c("B", "C"),
    logHR   = c(0.1, 0.2),
    selogHR = c(0.05, 0.05),
    stringsAsFactors = FALSE
  )
  res <- summarize_data(df_miss)
  expect_equal(res$missing_percent, 10)
})

test_that("summarize_data handles empty treatment column gracefully", {
  df_no_treat <- data.frame(
    study = c("Study1", "Study2"),
    val   = c(1, 2),
    stringsAsFactors = FALSE
  )
  res <- summarize_data(df_no_treat)
  expect_equal(res$n_studies, 2)
  expect_equal(res$n_arms, 2)
  expect_equal(res$n_treatments, 0)
})
