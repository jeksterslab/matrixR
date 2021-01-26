#' ---
#' title: "Test: pow"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: pow}
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
context("Test pow.")
#'
#' ## Parameters
#'
#+ parameters
A <- matrix(
  data = c(
    3, 4, 2, 5
  ),
  ncol = 2
)
#'
#' ## Results
#'
#+ results
B1 <- A %*% A %*% A
B2 <- pow(A, 3)
C1 <- solve(A) %*% solve(A) %*% solve(A)
C2 <- pow(A, -3)
D1 <- ones_from(A)
D2 <- pow(A, 0)
#'
#' ## testthat
#'
#+ testthat
test_that("B.", {
  for (i in seq_len(nrow(B1))) {
    for (j in seq_len(ncol(B1))) {
      expect_equal(
        B1[i, j],
        B2[i, j],
        check.attributes = FALSE
      )
    }
  }
})
test_that("B.", {
  for (i in seq_len(nrow(C1))) {
    for (j in seq_len(ncol(C1))) {
      expect_equal(
        C1[i, j],
        C2[i, j],
        check.attributes = FALSE
      )
    }
  }
})
test_that("B.", {
  for (i in seq_len(nrow(D1))) {
    for (j in seq_len(ncol(D1))) {
      expect_equal(
        D1[i, j],
        D2[i, j],
        check.attributes = FALSE
      )
    }
  }
})
