#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Identity Matrix
#'
#' @description Generates an indentity matrix.
#'
#' @param n Integer. Size.
#' @examples
#' ones(3)
#' @references
#'   [Wikipedia: Identity matrix](https://en.wikipedia.org/wiki/Identity_matrix)
#' @export
ones <- function(n) {
  return(
    diag(n)
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Identity Matrix from Dimensions of Another Square Matrix
#'
#' @description Generates an indentity matrix using the dimensions of another square matrix.
#'
#' @inheritParams is_sqr
#' @inherit ones references
#' @examples
#' A <- matrix(
#'   data = 1:9,
#'   ncol = 3
#' )
#' ones_from(A)
#' @export
ones_from <- function(A) {
  if (!is_sqr(A, chk.num = FALSE)) {
    stop(
      "`Input should be a square matrix."
    )
  }
  return(
    ones(dim(A)[1])
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Zero Matrix
#'
#' @description Generates a zero matrix.
#'
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
