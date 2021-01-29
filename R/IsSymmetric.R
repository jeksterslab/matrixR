#' Test for a Symmetric Matrix
#'
#' [IsSymmetric()] returns `TRUE` if `A` is a symmetric matrix,
#' and `FALSE` otherwise.
#'
#' An \eqn{m \times m} matrix \eqn{\mathbf{A}} is symmetric if
#'   \deqn{
#'     \mathbf{A}
#'     =
#'     \mathbf{A}^{\mathsf{T}} .
#'   }
#'
#' `A` is considered to be a symmetric matrix
#' if `all(A == t(A))` returns `TRUE`.
#'
#' @return Logical.
#' @author Ivan Jacob Agaloos Pesigan
#' @family predicate functions
#' @keywords predicate
#' @inheritParams IsMatrix
#' @examples
#' # TRUE
#' A <- matrix(
#'   data = c(1, 2, 3, 2, 4, 5, 3, 5, 6),
#'   ncol = 3
#' )
#' IsSymmetric(A)
#'
#' # FALSE
#' IsSymmetric(matrix(1:10, ncol = 2))
#' @export
IsSymmetric <- function(A) {
  UseMethod("IsSymmetric")
}

#' @rdname IsSymmetric
#' @export
IsSymmetric.default <- function(A) {
  if (IsSquareMatrix.default(A)) {
    return(all(A == t(A)))
  }
  return(FALSE)
}

#' @rdname IsSymmetric
#' @export
IsSymmetric.yac_symbol <- function(A) {
  out <- Ryacas::y_fn(A, "IsSymmetric")
  if (out$yacas_cmd == "True") {
    return(TRUE)
  }
  return(FALSE)
}
