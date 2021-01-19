#' ---
#' title: "Test: low2sym and up2sym"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: low2sym and up2sym}
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
context("Test low2sym and up2sym.")
#'
#' ## Parameters
#'
#+ results
A <- matrix(NA, ncol = 4, nrow = 4)
A[lower.tri(A, diag = TRUE)] <- 1:10
low2sym(A)
A <- matrix(NA, ncol = 4, nrow = 4)
A[upper.tri(A, diag = TRUE)] <- 1:10
up2sym(A)
is_sym(up2sym(A))
#'
#+ testhat
test_that("low2sym", {
  expect_true(
    is_sym(low2sym(A))
  )
})
test_that("up2sym", {
  expect_true(
    is_sym(up2sym(A))
  )
})
