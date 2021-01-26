#' ---
#' title: "Test: ones*"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: ones*}
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
context("Test ones*.")
#'
#' ## Results
#'
#+ results
A <- matrix(
  data = 1:9,
  ncol = 3
)
B <- ones(dim(A)[1])
B
C <- ones_from(A)
C
#'
#' ## testthat
#'
#+ testthat, echo=TRUE
test_that("I", {
  expect_equal(
    3,
    sum(B),
    sum(C)
  )
})
