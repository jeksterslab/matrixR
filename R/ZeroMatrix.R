#' Zero Matrix \eqn{\mathbf{0}}
#'
#' Generates a zero matrix.
#'
#' The \eqn{m \times n} zero matrix \eqn{\mathbf{0}}
#' is a matrix completely filled with zeroes.
#'
#' @return Numeric matrix.
#' @author Ivan Jacob Agaloos Pesigan
#' @family constructor functions
#' @keywords constructor
#' @param m Integer. Number of rows.
#' @param n Integer. Number of columns.
#'   If `NULL`, assumed `m = n` is assumed.
#' @examples
#' ZeroMatrix(3)
#' ZeroMatrix(3, 5)
#' @references
#'   [Wikipedia: Zero matrix](https://en.wikipedia.org/wiki/Zero_matrix)
#' @export
ZeroMatrix <- function(m, n = NULL) {
  if (is.null(n)) {
    n <- m
  }
  return(
    matrix(
      0,
      nrow = m,
      ncol = n
    )
  )
}

#' Zero Matrix \eqn{\mathbf{0}} from Dimensions of Another Matrix
#'
#' Generates a zero matrix
#' using the dimensions of an \eqn{m \times n} matrix \eqn{\mathbf{A}}.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family constructor functions
#' @keywords constructor
#' @inheritParams IsMatrix
#' @inherit ZeroMatrix details return references
#' @examples
#' ZeroMatrixFrom(matrix(1:9, ncol = 3))
#' @export
ZeroMatrixFrom <- function(A) {
  ZeroMatrix <- ZeroMatrix(dim(A)[1], dim(A)[2])
  colnames(ZeroMatrix) <- colnames(A)
  rownames(ZeroMatrix) <- rownames(A)
  return(ZeroMatrix)
}
