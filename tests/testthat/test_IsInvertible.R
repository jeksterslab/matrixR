#' ---
#' title: "Test: IsInvertible"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: IsInvertible}
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
context("Test IsInvertible.")
#'
#+ testthat
test_that("TRUE.", {
  expect_true(
    IsInvertible(diag(2))
  )
})
test_that("FALSE.", {
  expect_false(
    IsInvertible(matrix(1, nrow = 2, ncol = 2))
  )
  # to cover OTHERWISE
  expect_false(
    IsInvertible(matrix(1:10, ncol = 2))
  )
})
