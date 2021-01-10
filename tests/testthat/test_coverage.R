#' ---
#' title: "Test: Coverage"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: Coverage}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#'
#+ knitr_options, include=FALSE, cache=FALSE
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)
#'
#+ setup
library(testthat)
library(matrixR)
context("Test Coverage.")
#'
X <- matrix(
  data = 1:10,
  ncol = 2
)
test_that("nonsquare matrix", {
  expect_false(
    is_sqr(X)
  )
  expect_false(
    is_posdef(X)
  )
  expect_false(
    is_inv(X)
  )
})
X <- matrix(
  data = "text",
  nrow = 2,
  ncol = 2
)
test_that("nonnumeric", {
  expect_error(
    is_sqr(X)
  )
})
X <- 1:10
test_that("vector", {
  expect_error(
    is_sqr(X)
  )
})
