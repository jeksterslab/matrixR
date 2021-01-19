#' ---
#' title: "Test: cov2cor"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: cov2cor}
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
context("Test cov2cor.")
#'
#' ## Parameters
#'
#+ parameters
X <- MASS::mvrnorm(
  n = 10,
  mu = c(0, 0),
  Sigma = matrix(
    data = c(
      1,
      .5,
      .5,
      1
    ),
    ncol = 2
  )
)
A <- cov(X)
B <- cor(X)
colnames(B) <- paste0("col", 1:ncol(B))
rownames(B) <- paste0("row", 1:nrow(B))
v <- sqrt(diag(B))
knitr::kable(
  x = B,
  row.names = TRUE,
  caption = "Correlation Matrix"
)
#'
#' ## Results
#'
#+ results
C <- matrixR::cov2cor(A)
colnames(C) <- paste0("col", 1:ncol(C))
rownames(C) <- paste0("row", 1:nrow(C))
knitr::kable(
  x = C,
  row.names = TRUE,
  caption = "Correlation Matrix from cov2cor"
)
#'
#' ## testthat
#'
#+ testthat, echo = TRUE
test_that("B.", {
  for (i in 1:nrow(B)) {
    for (j in 1:ncol(B)) {
      expect_equal(
        B[i, j],
        C[i, j],
        check.attributes = FALSE
      )
    }
  }
})
