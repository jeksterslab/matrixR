#' ---
#' title: "Test: IsMatrix"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: IsMatrix}
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
context("Test IsMatrix.")
#'
#+ testthat
test_that("TRUE.", {
  expect_true(
    IsMatrix(matrix(1:10, ncol = 2))
  )
  expect_true(
    IsMatrix(matrix(1:10, nrow = 2))
  )
  # yac_symbol
  expect_true(
    IsMatrix(Ryacas::ysym(matrix(1:10, ncol = 2)))
  )
  expect_true(
    IsMatrix(Ryacas::ysym(matrix(1:10, nrow = 2)))
  )
})
test_that("FALSE.", {
  expect_false(
    IsMatrix(1:5)
  )
  # yac_symbol
  expect_false(
    IsMatrix(Ryacas::ysym(1:5))
  )
})
