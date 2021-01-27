#' ---
#' title: "Test: is_posdef"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: is_posdef}
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
context("Test is_posdef.")
#'
#' ## Parameters
#'
#+ parameters
matrix_T <- diag(2)
matrix_F <- matrix(
  data = c(1, 2, 2, 1),
  ncol = 2
)
colnames(matrix_T) <- paste0("col", 1:ncol(matrix_T))
rownames(matrix_T) <- paste0("row", 1:nrow(matrix_T))
colnames(matrix_F) <- paste0("col", 1:ncol(matrix_F))
rownames(matrix_F) <- paste0("row", 1:nrow(matrix_F))
knitr::kable(
  x = matrix_F,
  row.names = TRUE,
  caption = "Non-Positive Definite Matrix"
)
knitr::kable(
  x = matrix_T,
  row.names = TRUE,
  caption = "Positive Definite Matrix"
)
#'
#' ## Results
#'
#+ results
knitr::kable(
  x = data.frame(
    Item = c(
      "Non-Positive Definite Matrix",
      "Positive Definite Matrix"
    ),
    Parameter = c(
      "FALSE",
      "TRUE"
    ),
    Results = c(
      is_posdef(matrix_F),
      is_posdef(matrix_T)
    )
  ),
  row.names = FALSE
)
#'
#' ## testthat
#'
#+ testthat_01
test_that("is_posdef is TRUE", {
  expect_true(
    is_posdef(matrix_T)
  )
})
#'
#+ testthat_02
test_that("is_posdef is FALSE", {
  expect_false(
    is_posdef(matrix_F)
  )
})
