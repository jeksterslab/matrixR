#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Least Squares
#'
#' @description Least squares solution
#'   for \eqn{\mathbf{A} \mathbf{x} = \mathbf{b}}
#'
#' @details The least squares solution in given by
#'   \deqn{
#'     \mathbf{A}
#'     \mathbf{x}
#'     =
#'     \mathbf{b} \\
#'     \mathbf{A}^{\mathsf{T}}
#'     \mathbf{A}
#'     \mathbf{x}
#'     =
#'     \mathbf{A}^{\mathsf{T}}
#'     \mathbf{b} \\
#'     \mathbf{x}
#'     =
#'     \left(
#'       \mathbf{A}^{\mathsf{T}}
#'       \mathbf{A}
#'     \right)^{-1}
#'     \mathbf{A}^{\mathsf{T}}
#'     \mathbf{b}
#'   }
#'
#' @param A `m by n` numeric matrix.
#' @param b Numeric vector of length `m` or `m by 1` matrix.
#' @examples
#' X <- MASS::mvrnorm(
#'   n = 100,
#'   mu = c(0, 0),
#'   Sigma = matrix(
#'     data = c(1, .5, .5, 1),
#'     ncol = 2
#'   ),
#' )
#' A <- cbind(1, X[, 1])
#' b <- X[, 2]
#' LS(A, b)
#' @export
LS <- function(A,
               b) {
  return(
    solve(
      crossprod(A),
      crossprod(A, b)
    )
  )
  # alternative
  # solve(t(A) %*% A) %*% (t(A) %*% b)
}
