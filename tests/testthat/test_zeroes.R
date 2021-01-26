#' ---
#' title: "Test: zeroes*"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: zeroes*}
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
context("Test zeroes*.")
#'
#' ## Results
#'
#+ results
A <- matrix(
  data = 1:10,
  ncol = 2
)
B <- zeroes(dim(A)[1], dim(A)[2])
B
C <- zeroes_from(A)
C
#'
#' ## testthat
#'
#+ testthat, echo=TRUE
test_that("zeroes", {
  expect_equal(
    0,
    sum(B),
    sum(C)
  )
})
