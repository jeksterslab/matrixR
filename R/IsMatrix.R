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
#'   \deqn{
#'     \mathbf{A}
#'     =
#'     \begin{pmatrix}
#'       a_{1, 1} & a_{1, 2} & a_{1, 3} & \cdots & a_{1, n} \\
#'       a_{2, 1} & a_{2, 2} & a_{2, 3} & \cdots & a_{2, n} \\
#'       \vdots   & \vdots   & \vdots   & \cdots & \vdots   \\
#'       a_{i, j} & a_{i, j} & a_{i, j} & \cdots & a_{i, j} \\
#'       \vdots   & \vdots   & \vdots   & \vdots & \vdots   \\
#'       a_{m, 1} & a_{m, 2} & a_{m, 3} & \cdots & a_{m, n}
#'     \end{pmatrix}
#'     =
#'     \left( a_{i, j} \in \mathbf{R}^{m \times n} \right)
#'   }
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
