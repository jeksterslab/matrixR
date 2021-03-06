#' ---
#' title: "Test: IsPositiveDefinite"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: IsPositiveDefinite}
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
context("Test IsPositiveDefinite.")
#'
#+ testthat
test_that("TRUE.", {
  expect_true(
    IsPositiveDefinite(diag(2))
  )
})
test_that("FALSE.", {
  expect_false(
    IsPositiveDefinite(
      matrix(
        data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
        ncol = 3
      )
    )
  )
  # to cover OTHERWISE
  expect_false(
    IsPositiveDefinite(matrix(1:10, ncol = 2))
  )
})
