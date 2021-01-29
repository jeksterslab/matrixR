#' Test for a Positive Definite Matrix
#'
#' [IsPositiveDefinite()] returns `TRUE` if `A`
#' is a positive definite matrix,
#' and `FALSE` otherwise.
#'
#' An \eqn{m \times m} symmetric matrix \eqn{\mathbf{A}}
#' is positive definite
#' if all of its eigenvalues are positive.
#'
#' `A` is considered to be a positive definite matrix
#' if **NONE** of its eigenvalues are less than or equal to
#' a tolerance value.
#'
#' @return Logical.
#' @author Ivan Jacob Agaloos Pesigan
#' @family predicate functions
#' @keywords predicate
#' @inheritParams IsDiagonal
#' @examples
#' # TRUE
#' IsPositiveDefinite(diag(2))
#'
#' # FALSE
#' IsPositiveDefinite(matrix(1:9, ncol = 3))
#' @export
IsPositiveDefinite <- function(A,
                               tol = 1e-8) {
  if (IsSymmetric.default(A)) {
    return(
      isFALSE(
        any(eigen(A, only.values = TRUE)$values <= tol)
      )
    )
  }
  return(FALSE)
}
