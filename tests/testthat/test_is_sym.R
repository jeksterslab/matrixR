#' ---
#' title: "Test: is_sym"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: is_sym}
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
context("Test is_sym.")
#'
#' ## Parameters
#'
#+ parameters
matrix_T <- matrix(
  data = c(
    225, 112.50, 56.25,
    112.5, 225, 112.5,
    56.25, 112.50, 225
  ),
  ncol = 3
)
matrix_F1 <- matrix(
  data = 1:10,
  ncol = 2
)
matrix_F2 <- matrix(
  data = c(
    225, 0, 0,
    112.5, 225, 0,
    56.25, 112.50, 225
  ),
  ncol = 3
)
colnames(matrix_T) <- paste0("col", 1:ncol(matrix_T))
rownames(matrix_T) <- paste0("row", 1:nrow(matrix_T))
colnames(matrix_F1) <- paste0("col", 1:ncol(matrix_F1))
rownames(matrix_F1) <- paste0("row", 1:nrow(matrix_F1))
colnames(matrix_F2) <- paste0("col", 1:ncol(matrix_F2))
rownames(matrix_F2) <- paste0("row", 1:nrow(matrix_F2))
knitr::kable(
  x = matrix_F1,
  row.names = FALSE,
  caption = "Non-Square Matrix"
)
knitr::kable(
  x = matrix_F2,
  row.names = FALSE,
  caption = "Square but not Symmetric Matrix"
)
knitr::kable(
  x = matrix_T,
  row.names = FALSE,
  caption = "Square Matrix"
)
#'
#' ## Results
#'
#+ results
knitr::kable(
  x = data.frame(
    Item = c(
      "Non-Square Matrix",
      "Square but not Symmetric Matrix",
      "Square Matrix"
    ),
    Parameter = c(
      "FALSE",
      "FALSE",
      "TRUE"
    ),
    Results = c(
      is_sym(matrix_F1),
      is_sym(matrix_F2),
      is_sym(matrix_T)
    )
  ),
  row.names = FALSE
)
#'
#' ## testthat
#'
#+ testthat_01, echo=TRUE
test_that("is_sym is TRUE", {
  expect_true(
    is_sym(matrix_T)
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("is_sym is FALSE", {
  expect_false(
    is_sym(matrix_F1),
    is_sym(matrix_F2)
  )
})
