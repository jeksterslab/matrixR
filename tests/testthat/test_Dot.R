#' ---
#' title: "Test: Dot"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output: rmarkdown::html_vignette
#' vignette: >
#'   %\VignetteIndexEntry{Test: Dot}
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
context("Test Dot.")
#'
#+ testthat
test_that("Dot.", {
  expect_equal(
    Dot(
      c(1, 3, -5),
      c(4, -2, -1)
    ),
    as.matrix(3)
  )
  expect_equal(
    c(1, 3, -5) %.% c(4, -2, -1),
    as.matrix(3)
  )
  # yac_symbol
  expect_equal(
    as.numeric(
      Dot(
        Ryacas::ysym(c(1, 3, -5)),
        Ryacas::ysym(c(4, -2, -1))
      )
    ),
    3
  )
  expect_equal(
    eval(
      Dot(
        Ryacas::ysym(c(1, 3, -5)),
        Ryacas::ysym(c(4, -2, -1)),
        str = FALSE
      )
    ),
    3
  )
})
#'
#+ errors
test_that("Error.", {
  expect_error(
    Dot(
      c("a", "b", "c"),
      c("a", "b", "c")
    )
  )
  expect_error(
    Dot(
      c("a", "b", "c"),
      c("a", "b")
    )
  )
})
