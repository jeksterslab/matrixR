#' Test for a Vector
#'
#' [IsVector()] returns `TRUE` if `v` is a vector,
#' and `FALSE` otherwise.
#'
#' A vector is an array of numbers, symbols, or expressions,
#' typically denoted by lowercase roman letters such as
#' \eqn{\mathbf{v}} and \eqn{\mathbf{u}}.
#'
#' `v` is considered to be a vector
#' if `is.vector(v)` returns `TRUE` or
#' `v` is a `matrix`
#' and `dim(v)[1] == 1 | dim(v)[2] == 1` returns `TRUE`.
#'
#' @return Logical.
#' @author Ivan Jacob Agaloos Pesigan
#' @family predicate functions
#' @keywords predicate
#' @param v Input.
#' @references
#'   [Wikipedia: Row and column vectors](https://en.wikipedia.org/wiki/Row_and_column_vectors)
#' @examples
#' # TRUE
#' IsVector(1:5)
#' IsVector(matrix(1:5, ncol = 1))
#' IsVector(matrix(1:5, nrow = 1))
#'
#' # FALSE
#' IsVector(matrix(1:10, ncol = 2))
#' @export
IsVector <- function(v) {
  UseMethod("IsVector")
}

#' @rdname IsVector
#' @export
IsVector.default <- function(v) {
  if (is.vector(v)) {
    return(TRUE)
  }
  if (is.matrix(v)) {
    return(dim(v)[1] == 1 | dim(v)[2] == 1)
  }
  return(FALSE)
}

#' @rdname IsVector
#' @export
IsVector.yac_symbol <- function(v) {
  out <- Ryacas::y_fn(v, "IsVector")
  if (out$yacas_cmd == "True") {
    return(TRUE)
  }
  return(FALSE)
}
