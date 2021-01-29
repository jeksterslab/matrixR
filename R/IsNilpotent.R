#' Test for a Nilpotent Matrix
#'
#' [IsNilpotent()] returns `TRUE` if `A` is a nilpotent matrix,
#' and `FALSE` otherwise.
#'
#' An \eqn{m \times m} matrix \eqn{\mathbf{A}} is nilpotent
#' if all the eigenvalues are zero.
#'
#' `A` is considered to be a nilpotent matrix
#' if the absolute value of all eigenvalues
#' is less than or equal to a tolerance value.
#'
#' @return Logical.
#' @author Ivan Jacob Agaloos Pesigan
#' @family predicate functions
#' @keywords predicate
#' @inheritParams IsDiagonal
#' @references
#'   [Wikipedia: Nilpotent matrix](https://en.wikipedia.org/wiki/Nilpotent_matrix)
#' @examples
#' # TRUE
#' IsNilpotent(matrix(data = c(0, 0, 1, 0), ncol = 2))
#'
#' # FALSE
#' IsNilpotent(matrix(data = c(1, 0, 0, 0), ncol = 2))
#' @export
IsNilpotent <- function(A,
                        tol = 1e-8) {
  if (IsSquareMatrix.default(A)) {
    return(
      all(
        abs(eigen(A, only.values = TRUE)$values) <= tol
      )
    )
  }
  return(FALSE)
}
