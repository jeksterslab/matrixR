#' ---
#' title: "Test: Coverage"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: Coverage}
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
context("Test Coverage.")
#'
A <- matrix(
  data = 1:10,
  ncol = 2
)
test_that("nonsquare matrix", {
  expect_false(
    is_sqr(A)
  )
  expect_false(
    is_posdef(A)
  )
  expect_false(
    is_inv(A)
  )
  expect_false(
    is_diag(A)
  )
  expect_error(
    low2sym(A)
  )
  expect_error(
    up2sym(A)
  )
  expect_error(
    ones(dim(A)[1], dim(A)[2])
  )
  expect_error(
    ones_from(A)
  )
  expect_false(
    is_nilpot(A)
  )
  expect_false(
    is_idempot(A)
  )
  expect_error(
    pow(A, 0)
  )
  expect_error(
    tr(A)
  )
})
B <- matrix(
  data = "text",
  nrow = 2,
  ncol = 2
)
test_that("nonnumeric", {
  expect_error(
    is_sqr(B)
  )
})
C <- 1:10
test_that("vector", {
  expect_error(
    is_sqr(C)
  )
})
D <- matrix(
  data = c(1, 2, 2, 1),
  ncol = 2
)
v <- c(1, 2)
test_that("Cor2Cov - is_posdef", {
  expect_error(
    Cor2Cov(D, v)
  )
})
test_that("Cov2Cor - is_posdef", {
  expect_error(
    Cov2Cor(D)
  )
})
E <- matrix(
  data = c(
    1,
    .5,
    .5,
    1
  ),
  ncol = 2
)
u <- c(-1, 1)
test_that("Cor2Cov - v is positive", {
  expect_error(
    Cor2Cov(E, u)
  )
})
