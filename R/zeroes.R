#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Zero Matrix
#'
#' @description Generates a zero matrix.
#'
#' @family zeroes functions
#' @keywords zeroes
#' @param m Integer. Number of rows.
#' @param n Integer. Number of columns.
#' @examples
#' zeroes(3, 5)
#' @references
#'   [Wikipedia: Zero matrix](https://en.wikipedia.org/wiki/Zero_matrix)
#' @export
zeroes <- function(m,
                   n) {
  return(
    matrix(
      data = 0,
      nrow = m,
      ncol = n
    )
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Zero Matrix from Dimensions of Another Matrix
#'
#' @description Generates a zero matrix using the dimensions of another matrix.
#'
#' @family zeroes functions
#' @keywords zeroes
#' @inheritParams is_sqr
#' @inherit zeroes references
#' @examples
#' A <- matrix(
#'   data = 1:10,
#'   ncol = 2
#' )
#' zeroes_from(A)
#' @export
zeroes_from <- function(A) {
  return(
    zeroes(
      m = dim(A)[1],
      n = dim(A)[2]
    )
  )
}
