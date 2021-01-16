#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Correlation to Covariance
#'
#' @description Converts a correlation matrix to a covariance matrix.
#'
#' @details The covariance matrix is given by
#'
#'   \deqn{
#'     \left(
#'       \mathbf{v}
#'       \mathbf{I}_{p}
#'     \right)
#'     \mathbf{A}
#'     \left(
#'       \mathbf{v}
#'       \mathbf{I}_{p}
#'     \right)
#'   }
#'
#'   where
#'
#'   - \eqn{\mathbf{A}_{p \times p}} is a correlation matrix,
#'   - \eqn{\mathbf{I}_{p \times p}} is an identity matrix,
#'   - \eqn{\mathbf{v}_{p \times 1}} is a vector of standard deviations.
#'
#' @param A Numeric matrix.
#'   A \eqn{p \times p} positive definite correlation matrix.
#' @param v Numeric vector of length `p` or `p by 1` matrix.
#'   A vector of \eqn{p \times 1} standard deviations.
#' @return
#'   Returns a covariance matrix.
#' @examples
#' A <- matrix(
#'   data = c(
#'     1, 0.509902, 0.26,
#'     0.509902, 1, 0.509902,
#'     0.26, 0.509902, 1
#'   ),
#'   ncol = 3
#' )
#' v <- c(15, 15, 15)
#'
#' diag(v) %*% A %*% diag(v)
#'
#' cor2cov(A, v)
#' @export
cor2cov <- function(A,
                    v) {
  if (!is_posdef(A)) {
    stop(
      "Input matrix should be positive definite."
    )
  }
  if (any(v < 0)) {
    stop(
      "Standard deviations should all be positive real numbers."
    )
  }
  return(
    A * tcrossprod(v)
  )
  # another implementation
  # D <- diag(as.vector(v))
  # D %*% A %*% D
}

# cov2cor
# @param A variance-covariance matrix
# A * tcrossprod(
#  diag(
#    solve(
#      diag(
#        sqrt(
#          diag(A)
#        )
#      )
#    )
#   )
# )
# another implementation
# inverse of diagonal matrix of standard deviations
# invD <- solve(diag(sqrt(diag(A))))
# invD %*% A %*% invD
