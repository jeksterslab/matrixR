#' ---
#' title: "Test: IsNilpotent"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: IsNilpotent}
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
context("Test IsNilpotent.")
#'
#+ testthat
test_that("TRUE.", {
  expect_true(
    IsNilpotent(
      matrix(
        data = c(0, 0, 1, 0), ncol = 2
      )
    )
  )
  expect_true(
    IsNilpotent(
      matrix(
        data = c(0, 0, 0, 0, 2, 0, 0, 0, 1, 1, 0, 0, 6, 2, 3, 0), ncol = 4
      )
    )
  )
  expect_true(
    IsNilpotent(
      matrix(
        data = c(5, 15, 10, -3, -9, -6, 2, 6, 4), ncol = 3
      ),
      tol = 1e-6
    )
  )
  expect_true(
    IsNilpotent(
      matrix(
        data = c(5, 6, -11, 5, 6, -11, 5, 6, -11), ncol = 3
      ),
      tol = 1e-6
    )
  )
  expect_true(
    IsNilpotent(
      matrix(
        data = c(2, 4, -1, -2), ncol = 2
      )
    )
  )
})
test_that("FALSE.", {
  expect_false(
    IsNilpotent(
      matrix(
        data = c(1, 0, 0, 0), ncol = 2
      )
    )
  )
  # to cover OTHERWISE
  expect_false(
    IsNilpotent(matrix(1:10, ncol = 2))
  )
})
