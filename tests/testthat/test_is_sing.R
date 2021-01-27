#' ---
#' title: "Test: is_sing"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: is_sing}
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
context("Test is_sing.")
#'
#' ## Parameters
#'
#+ parameters
matrix_T <- matrix(
  data = 1,
  nrow = 2,
  ncol = 2
)
matrix_F <- diag(2)
colnames(matrix_T) <- paste0("col", 1:ncol(matrix_T))
rownames(matrix_T) <- paste0("row", 1:nrow(matrix_T))
colnames(matrix_F) <- paste0("col", 1:ncol(matrix_F))
rownames(matrix_F) <- paste0("row", 1:nrow(matrix_F))
knitr::kable(
  x = matrix_F,
  row.names = TRUE,
  caption = "Non-Singular Matrix"
)
knitr::kable(
  x = matrix_T,
  row.names = TRUE,
  caption = "Singular Matrix"
)
#'
#' ## Results
#'
#+ results
knitr::kable(
  x = data.frame(
    Item = c(
      "Non-Singular Matrix",
      "Singular Matrix"
    ),
    Parameter = c(
      "FALSE",
      "TRUE"
    ),
    Results = c(
      is_sing(matrix_F),
      is_sing(matrix_T)
    )
  ),
  row.names = FALSE
)
#'
#' ## testthat
#'
#+ testthat_01
test_that("is_sing is TRUE", {
  expect_true(
    is_sing(matrix_T)
  )
})
#'
#+ testthat_02
test_that("is_sing is FALSE", {
  expect_false(
    is_sing(matrix_F)
  )
})
