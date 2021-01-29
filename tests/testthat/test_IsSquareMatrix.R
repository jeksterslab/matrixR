#' ---
#' title: "Test: IsSquareMatrix"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: IsSquareMatrix}
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
context("Test IsSquareMatrix.")
#'
#+ testthat
test_that("TRUE.", {
  expect_true(
    IsSquareMatrix(matrix(1:9, ncol = 3))
  )
  # yac_symbol
  expect_true(
    IsSquareMatrix(Ryacas::ysym(matrix(1:9, ncol = 3)))
  )
})
test_that("FALSE.", {
  expect_false(
    IsSquareMatrix(matrix(1:10, ncol = 2))
  )
  # yac_symbol
  expect_false(
    IsSquareMatrix(Ryacas::ysym(matrix(1:10, ncol = 2)))
  )
  # to cover OTHERWISE
  expect_false(
    IsSquareMatrix(1:5)
  )
})
