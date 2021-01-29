#' Test for a Matrix
#'
#' [IsMatrix()] returns `TRUE` if `A` is a matrix,
#' and `FALSE` otherwise.
#'
#' A matrix is a rectangular array of numbers, symbols, or expressions,
#' with \eqn{m} rows by \eqn{n} columns
#' typically denoted by uppercase roman letters such as
#' \eqn{\mathbf{A}}, \eqn{\mathbf{B}}, and \eqn{\mathbf{C}}.
#'
#' @return Logical.
#' @author Ivan Jacob Agaloos Pesigan
#' @family predicate functions
#' @keywords predicate
#' @param A Input.
#' @references
#'   [Wikipedia: Matrix (mathematics)](https://en.wikipedia.org/wiki/Matrix_(mathematics))
#' @examples
#' # TRUE
#' IsMatrix(matrix(1:10, ncol = 2))
#' IsMatrix(matrix(1:10, nrow = 2))
#'
#' # FALSE
#' IsMatrix(1:5)
#' @export
IsMatrix <- function(A) {
  UseMethod("IsMatrix")
}

#' @rdname IsMatrix
#' @export
IsMatrix.default <- function(A) {
  return(base::is.matrix(A))
}

#' @rdname IsMatrix
#' @export
IsMatrix.yac_symbol <- function(A) {
  out <- Ryacas::y_fn(A, "IsMatrix")
  if (out$yacas_cmd == "True") {
    return(TRUE)
  }
  return(FALSE)
}
