#' ---
#' title: "Test: IsSymmetric"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: IsSymmetric}
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
context("Test IsSymmetric.")
#'
#+ testthat
test_that("TRUE.", {
  expect_true(
    IsSymmetric(
      matrix(
        data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
        ncol = 3
      )
    )
  )
  # yac_symbol
  expect_true(
    IsSymmetric(
      Ryacas::ysym(matrix(
        data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
        ncol = 3
      ))
    )
  )
})
test_that("FALSE.", {
  expect_false(
    IsSymmetric(matrix(1:9, nrow = 3))
  )
  # yac_symbol
  expect_false(
    IsSymmetric(Ryacas::ysym(matrix(1:9, nrow = 3)))
  )
  # to cover OTHERWISE
  expect_false(
    IsSymmetric(matrix(1:10, nrow = 2))
  )
})
