#' ---
#' title: "Test: IsOrthogonal"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: IsOrthogonal}
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
context("Test IsOrthogonal.")
#'
#+ testthat
test_that("TRUE.", {
  expect_true(
    IsOrthogonal(matrix(c(1, 0, 0, 1), ncol = 2))
  )
  expect_true(
    IsOrthogonal(matrix(c(1, 0, 0, -1), ncol = 2))
  )
  expect_true(
    IsOrthogonal(
      matrix(
        c(0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0),
        ncol = 4
      )
    )
  )
  # yac_symbol
  expect_true(
    IsOrthogonal(Ryacas::ysym(matrix(c(1, 0, 0, 1), ncol = 2)))
  )
  expect_true(
    IsOrthogonal(Ryacas::ysym(matrix(c(1, 0, 0, -1), ncol = 2)))
  )
  expect_true(
    IsOrthogonal(
      Ryacas::ysym(
        matrix(c(0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0), ncol = 4)
      )
    )
  )
})
test_that("FALSE.", {
  expect_false(
    IsOrthogonal(matrix(data = 1, nrow = 2, ncol = 2))
  )
  # yac_symbol
  expect_false(
    IsOrthogonal(Ryacas::ysym(matrix(data = 1, nrow = 2, ncol = 2)))
  )
  # to cover OTHERWISE
  expect_false(
    IsOrthogonal(matrix(1:10, ncol = 2))
  )
})
