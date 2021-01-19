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
A
B <- low2sym(A)
B
C <- matrix(NA, ncol = 4, nrow = 4)
C[upper.tri(C, diag = TRUE)] <- 1:10
C
D <- up2sym(C)
D
#'
#+ testhat
test_that("low2sym", {
  expect_true(
    is_sym(B)
  )
})
test_that("up2sym", {
  expect_true(
    is_sym(D)
  )
})
