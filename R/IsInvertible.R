#' Test for an Invertible Matrix
#'
#' [IsInvertible()] returns `TRUE` if `A` is an invertible matrix,
#' and `FALSE` otherwise.
#'
#' An \eqn{m \times m} matrix \eqn{\mathbf{A}} is invertible
#' if there exists an \eqn{m \times m} matrix \eqn{\mathbf{B}}
#' such that
#'   \deqn{
#'     \mathbf{A} \mathbf{B}
#'     =
#'     \mathbf{B}
#'     \mathbf{A}
#'     =
#'     \mathbf{I}_{m}
#'   }
#' where \eqn{\mathbf{I}_{m}} is an \eqn{m \times m} identity matrix.
#'
#' An \eqn{m \times m} matrix \eqn{\mathbf{A}} is noninvertible
#' or singular if its determinant is zero.
#'
#' `A` is considered invertible if its determinant
#' is **NOT** less than or equal
#' to a tolerance value.
#'
#' @return Logical.
#' @author Ivan Jacob Agaloos Pesigan
#' @family predicate functions
#' @keywords predicate
#' @inheritParams IsDiagonal
#' @examples
#' # TRUE
#' IsInvertible(diag(2))
#'
#' # FALSE
#' IsInvertible(matrix(1, nrow = 2, ncol = 2))
#' @export
IsInvertible <- function(A,
                         tol = 1e-8) {
  if (IsSquareMatrix.default(A)) {
    return(
      isFALSE(
        det(A) <= tol
      )
    )
  }
  return(FALSE)
}
