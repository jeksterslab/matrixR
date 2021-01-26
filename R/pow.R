#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Matrix Power
#'
#' @description Calculates the power of a matrix.
#'
#' @details
#'   \deqn{
#'     \mathbf{A}^{n}
#'   }
#'
#' @inheritParams is_sqr
#' @param n Integer. Power.
#' @examples
#' A <- matrix(
#'   data = c(
#'     3, 4, 2, 5
#'   ),
#'   ncol = 2
#' )
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
  return(
    out
  )
}
