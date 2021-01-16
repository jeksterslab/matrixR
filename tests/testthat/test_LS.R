#' ---
#' title: "Test: LS"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: LS}
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
context("Test LS.")
#'
#' ## Parameters
#'
#+ parameters
X <- MASS::mvrnorm(
  n = 100,
  mu = c(0, 0),
  Sigma = matrix(
    data = c(1, .5, .5, 1),
    ncol = 2
  )
)
A <- cbind(1, X[, 1])
b <- X[, 2]
params <- as.vector(coef(lm(X[, 2] ~ X[, 1])))
#'
#' ## Results
#'
#+ results
x <- as.vector(LS(A, b))
test_that("LS.", {
  for (i in seq_along(params)) {
    expect_equal(
      params[i],
      x[i],
      check.attributes = FALSE
    )
  }
})
