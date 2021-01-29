#' Test for a Diagonal Matrix
#'
#' [IsDiagonal()] returns `TRUE` if `A` is a diagonal matrix,
#' and `FALSE` otherwise.
#'
#' An \eqn{m \times m} matrix \eqn{\mathbf{A}} is diagonal
#' if all off-diagonal elements are zero.
#'
#' `A` is considered to be a diagonal matrix
#' if the absolute value of all off-diagonal elements
#' is less than or equal to a tolerance value.
#'
#' @return Logical.
#' @author Ivan Jacob Agaloos Pesigan
#' @family predicate functions
#' @keywords predicate
#' @inheritParams IsMatrix
#' @param ... ...
#' @examples
#' # TRUE
#' IsDiagonal(diag(2))
#'
#' # FALSE
#' IsDiagonal(matrix(1:9, ncol = 3))
#' @references
#'   [Wikipedia: Diagonal matrix](https://en.wikipedia.org/wiki/Diagonal_matrix)
#' @export
IsDiagonal <- function(A,
                       ...) {
  UseMethod("IsDiagonal")
}

#' @rdname IsDiagonal
#' @param tol Numeric. Tolerance.
#' @export
IsDiagonal.default <- function(A,
                               tol = 1e-8,
                               ...) {
  if (IsSquareMatrix.default(A)) {
    diag(A) <- 0
    return(
      all(abs(A) <= tol)
    )
  }
  return(FALSE)
}

#' @rdname IsDiagonal
#' @export
IsDiagonal.yac_symbol <- function(A,
                                  ...) {
  out <- Ryacas::y_fn(A, "IsDiagonal")
  if (out$yacas_cmd == "True") {
    return(TRUE)
  }
  return(FALSE)
}
