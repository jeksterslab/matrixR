#' ---
#' title: "Test: IsIdempotent"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: IsIdempotent}
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
context("Test IsIdempotent.")
#'
#+ testthat
test_that("TRUE.", {
  expect_true(
    IsIdempotent(
      matrix(
        data = c(1, 0, 0, 1),
        ncol = 2
      )
    )
  )
  expect_true(
    IsIdempotent(
      matrix(
        data = c(3, 1, -6, -2),
        ncol = 2
      )
    )
  )
  expect_true(
    IsIdempotent(
      matrix(
        data = c(1, 0, 0, 0, 1, 0, 0, 0, 1),
        ncol = 3
      )
    )
  )
  expect_true(
    IsIdempotent(
      matrix(
        data = c(2, -1, 1, -2, 3, -2, -4, 4, -3),
        ncol = 3
      )
    )
  )
  # yac_symbol
  expect_true(
    IsIdempotent(
      Ryacas::ysym(
        matrix(
          data = c(1, 0, 0, 1),
          ncol = 2
        )
      )
    )
  )
  expect_true(
    IsIdempotent(
      Ryacas::ysym(
        matrix(
          data = c(3, 1, -6, -2),
          ncol = 2
        )
      )
    )
  )
  expect_true(
    IsIdempotent(
      Ryacas::ysym(
        matrix(
          data = c(1, 0, 0, 0, 1, 0, 0, 0, 1),
          ncol = 3
        )
      )
    )
  )
  expect_true(
    IsIdempotent(
      Ryacas::ysym(
        matrix(
          data = c(2, -1, 1, -2, 3, -2, -4, 4, -3),
          ncol = 3
        )
      )
    )
  )
})
test_that("FALSE.", {
  expect_false(
    IsIdempotent(
      matrix(data = 1, nrow = 2, ncol = 2)
    )
  )
  expect_false(
    IsIdempotent(
      Ryacas::ysym(matrix(data = 1, nrow = 2, ncol = 2))
    )
  )
  # to cover OTHERWISE
  expect_false(
    IsIdempotent(
      matrix(data = 1:10, ncol = 2)
    )
  )
})
