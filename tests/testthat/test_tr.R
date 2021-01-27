#' ---
#' title: "Test: tr"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: tr}
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
context("Test tr.")
#'
#' ## Results
#'
#+ results
A <- matrix(
  data = c(
    1, 11, 6,
    0, 5, 12,
    3, 2, -5
  ),
  ncol = 3
)
x <- tr(A)
x
#'
#+ testhat
test_that("tr.", {
  expect_equal(
    1,
    x,
    check.attributes = FALSE
  )
})
