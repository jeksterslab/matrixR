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
#'       \mathbf{I}
#'     \right)
#'     \mathbf{A}
#'     \left(
#'       \mathbf{v}
#'       \mathbf{I}
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

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Covariance to Correlation
#'
#' @description Converts a covariance matrix to a correlation matrix.
#'
#' @details The correlation matrix is given by
#'
#'   \deqn{
#'     \left(
#'       \mathbf{v}
#'       \mathbf{I}
#'     \right)^{-1}
#'     \mathbf{A}
#'     \left(
#'       \mathbf{v}
#'       \mathbf{I}
#'     \right)^{-1}
#'   }
#'
#'   where
#'
#'   - \eqn{\mathbf{A}_{p \times p}} is a covariance matrix,
#'   - \eqn{\mathbf{I}_{p \times p}} is an identity matrix,
#'   - \eqn{\mathbf{v}_{p \times 1}} is a vector of standard deviations,
#'     that is, the square root of the diagonal elements of \eqn{\mathbf{A}}.
#'
#' @param A Numeric matrix.
#'   A \eqn{p \times p} positive definite covariance matrix.
#' @return
#'   Returns a correlation matrix.
#' @examples
#' A <- matrix(
#'   data = c(
#'     225.0000, 114.7279, 58.5000,
#'     114.7279, 225.0000, 114.7279,
#'     58.5000, 114.7279, 225.0000
#'   ),
#'   ncol = 3
#' )
#'
#' stats::cov2cor(A)
#'
#' matrixR::cov2cor(A)
#' @export
cov2cor <- function(A) {
  if (!is_posdef(A)) {
    stop(
      "Input matrix should be positive definite."
    )
  }
  return(
    A * tcrossprod(
      diag(
        solve(
          diag(
            sqrt(
              diag(A)
            )
          )
        )
      )
    )
  )
  # another implementation
  # inverse of diagonal matrix of standard deviations
  # invD <- solve(diag(sqrt(diag(A))))
  # invD %*% A %*% invD
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Lower Triangle to Symmetric
#'
#' @description Creates a symmetric matrix from the lower triangle
#'   of a square matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams is_sqr
#' @return Returns a symmetric matrix.
#' @examples
#' A <- matrix(NA, ncol = 4, nrow = 4)
#' A[lower.tri(A, diag = TRUE)] <- 1:10
#' A
#' B <- low2sym(A)
#' is_sym(B)
#' @export
low2sym <- function(A,
                    chk.num = TRUE) {
  if (!is_sqr(A, chk.num)) {
    stop(
      "Input should be a square matrix."
    )
  }
  A[upper.tri(A)] <- t(A)[upper.tri(A)]
  return(A)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Upper Triangle to Symmetric
#'
#' @description Creates a symmetric matrix from the upper triangle
#'   of a square matrix.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams is_sqr
#' @return Returns a symmetric matrix.
#' @examples
#' A <- matrix(NA, ncol = 4, nrow = 4)
#' A[upper.tri(A, diag = TRUE)] <- 1:10
#' A
#' B <- up2sym(A)
#' is_sym(B)
#' @export
up2sym <- function(A,
                   chk.num = TRUE) {
  if (!is_sqr(A, chk.num)) {
    stop(
      "Input should be a square matrix."
    )
  }
  A[lower.tri(A)] <- t(A)[lower.tri(A)]
  return(A)
}
