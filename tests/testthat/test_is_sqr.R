#' ---
#' title: "Test: is_sqr"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: is_sqr}
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
context("Test is_sqr.")
#'
#' ## Parameters
#'
#+ parameters
matrix_T <- matrix(
  data = 1:9,
  nrow = 3
)
matrix_F <- matrix(
  data = 1:10,
  ncol = 2
)
colnames(matrix_T) <- paste0("col", 1:ncol(matrix_T))
rownames(matrix_T) <- paste0("row", 1:nrow(matrix_T))
colnames(matrix_F) <- paste0("col", 1:ncol(matrix_F))
rownames(matrix_F) <- paste0("row", 1:nrow(matrix_F))
knitr::kable(
  x = matrix_F,
  row.names = TRUE,
  caption = "Non-Square Matrix"
)
knitr::kable(
  x = matrix_T,
  row.names = TRUE,
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
      "Square Matrix"
    ),
    Parameter = c(
      "FALSE",
      "TRUE"
    ),
    Results = c(
      is_sqr(matrix_F),
      is_sqr(matrix_T)
    )
  ),
  row.names = FALSE
)
#'
#' ## testthat
#'
#+ testthat_01, echo=TRUE
test_that("is_sqr is TRUE", {
  expect_true(
    is_sqr(matrix_T)
  )
})
#'
#+ testthat_02, echo=TRUE
test_that("is_sqr is FALSE", {
  expect_false(
    is_sqr(matrix_F)
  )
})
