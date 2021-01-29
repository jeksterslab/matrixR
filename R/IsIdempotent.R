#' Test for an Idempotent Matrix
#'
#' [IsIdempotent()] returns `TRUE` if `A` is an idempotent matrix,
#' and `FALSE` otherwise.
#'
#' An \eqn{m \times m} matrix \eqn{\mathbf{A}} is idempotent if
#'   \deqn{
#'     \mathbf{A}^2 = \mathbf{A} .
#'   }
#'
#' `A` is considered to be an idempotent matrix
#' if `all(abs(A %*% A - A) <= tol)` returns `TRUE`.
#'
#' @return Logical.
#' @author Ivan Jacob Agaloos Pesigan
#' @family predicate functions
#' @keywords predicate
#' @inheritParams IsDiagonal
#' @examples
#' # TRUE
#' IsIdempotent(matrix(data = c(1, 0, 0, 1), ncol = 2))
#'
#' # FALSE
#' IsIdempotent(matrix(data = 1, nrow = 2, ncol = 2))
#' @export
IsIdempotent <- function(A,
                         ...) {
  UseMethod("IsIdempotent")
}

#' @rdname IsIdempotent
#' @inheritParams IsDiagonal
#' @export
IsIdempotent.default <- function(A,
                                 tol = 1e-8,
                                 ...) {
  if (IsSquareMatrix.default(A)) {
    return(
      all(abs(A %*% A - A) <= tol)
    )
  }
  return(FALSE)
}

#' @rdname IsIdempotent
#' @export
IsIdempotent.yac_symbol <- function(A,
                                    ...) {
  out <- Ryacas::y_fn(A, "IsIdempotent")
  if (out$yacas_cmd == "True") {
    return(TRUE)
  }
  return(FALSE)
}
