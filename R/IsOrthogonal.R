#' Test for an Orthogonal Matrix
#'
#' [IsOrthogonal()] returns `TRUE` if `A` is an orthogonal matrix,
#' and `FALSE` otherwise.
#'
#' An \eqn{m \times m} matrix \eqn{\mathbf{A}} is orthogonal if
#'   \deqn{
#'     \mathbf{A}^{\mathsf{T}} \mathbf{A}
#'     =
#'     \mathbf{A} \mathbf{A}^{\mathsf{T}}
#'     \mathbf{I} .
#'   }
#'
#' Or equivalently
#'   \deqn{
#'     \mathbf{A}^{\mathsf{T}}
#'     =
#'     \mathbf{A}^{-1} .
#'   }
#'
#' `A` is considered to be an orthogonal matrix
#' if `all(abs(crossprod(A) - diag(dim(A)[1])) <= tol)` returns `TRUE`.
#'
#' @return Logical.
#' @author Ivan Jacob Agaloos Pesigan
#' @family predicate functions
#' @keywords predicate
#' @inheritParams IsDiagonal
#' @examples
#' # TRUE
#' IsOrthogonal(matrix(data = c(1, 0, 0, 1), ncol = 2))
#'
#' # FALSE
#' IsOrthogonal(matrix(data = 1, nrow = 2, ncol = 2))
#' @export
IsOrthogonal <- function(A,
                         ...) {
  UseMethod("IsOrthogonal")
}

#' @rdname IsOrthogonal
#' @inheritParams IsDiagonal
#' @export
IsOrthogonal.default <- function(A,
                                 tol = 1e-8,
                                 ...) {
  if (IsSquareMatrix.default(A)) {
    return(
      all(abs(crossprod(A) - diag(dim(A)[1])) <= tol)
    )
  }
  return(FALSE)
}

#' @rdname IsOrthogonal
#' @export
IsOrthogonal.yac_symbol <- function(A,
                                    ...) {
  out <- Ryacas::y_fn(A, "IsOrthogonal")
  if (out$yacas_cmd == "True") {
    return(TRUE)
  }
  return(FALSE)
}
