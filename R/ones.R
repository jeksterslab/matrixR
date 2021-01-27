#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Identity Matrix
#'
#' @description Generates an indentity matrix.
#'
#' @family ones functions
#' @keywords ones
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
#' @family ones functions
#' @keywords ones
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
