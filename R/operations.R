#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Matrix Power
#'
#' @description Calculates the power of a square matrix.
#'
#' @details The power of a matrix is given by
#'
#'   \deqn{
#'     \mathbf{A}^{n}
#'   }
#'
#'   Let \eqn{n} be 0
#'
#'   \deqn{
#'     \mathbf{A}^{0}
#'     =
#'     \mathbf{A} .
#'   }
#'
#'   Let \eqn{n} be a positive integer for example \eqn{2}
#'
#'   \deqn{
#'     \mathbf{A}^{2}
#'     =
#'     \mathbf{A} \cdot \mathbf{A} .
#'   }
#'
#'   Let \eqn{n} be a negative integer for example \eqn{-2}
#'
#'   \deqn{
#'     \mathbf{A}^{-2}
#'     =
#'     \left( \mathbf{A}^{-1} \right)^2
#'     =
#'     \left( \mathbf{A}^{-1} \right) \cdot \left( \mathbf{A}^{-1} \right) .
#'   }
#'
#' @family operation functions
#' @keywords operation
#' @inheritParams is_sqr
#' @param n Integer. Power.
#' @examples
#' A <- matrix(
#'   data = c(
#'     225, 112.50, 56.25,
#'     112.5, 225, 112.5,
#'     56.25, 112.50, 225
#'   ),
#'   ncol = 3
#' )
#'
#' A %*% A
#'
#' pow(A, 2)
#' @export
pow <- function(A, n) {
  if (!is_sqr(A)) {
    stop(
      "Input should be a square matrix"
    )
  }
  n <- as.integer(n)
  if (n == 0) {
    return(ones_from(A))
  }
  if (n < 0) {
    A <- solve(A)
    n <- abs(n)
  }
  out <- A
  iter <- n - 1
  for (i in 1:iter) {
    out <- out %*% A
  }
  return(out)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Matrix Trace
#'
#' @description Calculates the trace of a matrix.
#'
#' @details The trace of an \eqn{n \times n} matrix \eqn{\mathbf{A}}
#'   is given by
#'
#'   \deqn{
#'     \mathrm{Tr} \left( A \right)
#'     =
#'     \sum_{i = 1}^{n} a_{ii}
#'     =
#'     a_{11} + a_{22} + \cdots + a_{nn}
#'   }
#'
#' @family operation functions
#' @keywords operation
#' @inheritParams is_sqr
#' @return Returns the the trace of a matrix.
#' @examples
#' A <- matrix(
#'   data = c(
#'     225, 112.50, 56.25,
#'     112.5, 225, 112.5,
#'     56.25, 112.50, 225
#'   ),
#'   ncol = 3
#' )
#' tr(A)
#' @references
#'   [Wikipedia: Square matrix](https://en.wikipedia.org/wiki/Trace_(linear_algebra))
#' @export
tr <- function(A) {
  if (!is_sqr(A)) {
    stop(
      "Input should be a square matrix."
    )
  }
  return(sum(diag(A)))
}
