#' Test for a Singular Matrix
#'
#' [IsInvertible()] returns `TRUE` if `A` is a singular matrix,
#' and `FALSE` otherwise.
#'
#' An \eqn{m \times m} matrix \eqn{\mathbf{A}} is singular
#' if its determinant is zero.
#'
#' The function compares the determinant of \eqn{\mathbf{A}}
#' to a tolerance value.
#' If the determinant is less than or equal
#' to a tolerance value,
#' the matrix is assumed to be singular.
#'
#' @return Logical.
#' @author Ivan Jacob Agaloos Pesigan
#' @family predicate functions
#' @keywords predicate
#' @inheritParams IsDiagonal
#' @examples
#' # TRUE
#' IsSingular(matrix(1, nrow = 2, ncol = 2))
#'
#' # FALSE
#' IsSingular(diag(2))
#' @export
IsSingular <- function(A,
                       tol = 1e-8) {
  if (IsSquareMatrix.default(A)) {
    return(det(A) <= tol)
  }
  return(FALSE)
}
