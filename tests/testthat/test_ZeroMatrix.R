#' ---
#' title: "Test: ZeroMatrix"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: ZeroMatrix}
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
context("Test ZeroMatrix.")
#'
A <- matrix(1:9, ncol = 3)
#'
#+ testthat
test_that("ZeroMatrix", {
  expect_equal(
    0,
    sum(ZeroMatrix(dim(A)[1])),
    sum(ZeroMatrixFrom(A))
  )
})
