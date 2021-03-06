#' ---
#' title: "Test: IsDiagonal"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: IsDiagonal}
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
context("Test IsDiagonal.")
#'
#+ testthat
test_that("TRUE.", {
  expect_true(
    IsDiagonal(diag(2))
  )
  # yac_symbol
  expect_true(
    IsDiagonal(Ryacas::ysym(diag(2)))
  )
})
test_that("FALSE.", {
  expect_false(
    IsDiagonal(matrix(1:9, ncol = 3))
  )
  expect_false(
    IsDiagonal(Ryacas::ysym(matrix(1:9, ncol = 3)))
  )
  # to cover OTHERWISE
  expect_false(
    IsDiagonal(matrix(1:10, ncol = 2))
  )
})
