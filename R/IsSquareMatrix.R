#' Test for a Square Matrix
#'
#' [IsSquareMatrix()] returns `TRUE` if `A` is a square matrix,
#' and `FALSE` otherwise.
#'
#' A matrix \eqn{\mathbf{A}} is square
#' if it has the same number of rows and columns
#'   \deqn{
#'     \mathbf{A}_{m \times m} .
#'   }
#'
#' `A` is considered to be a square matrix
#' if `dim(A)[1] == dim(A)[2]` returns `TRUE`.
#'
#' @return Logical.
#' @author Ivan Jacob Agaloos Pesigan
#' @family predicate functions
#' @keywords predicate
#' @inheritParams IsMatrix
#' @references
#'   [Wikipedia: Square matrix](https://en.wikipedia.org/wiki/Square_matrix)
#' @examples
#' # TRUE
#' IsSquareMatrix(matrix(1:9, ncol = 3))
#'
#' # FALSE
#' IsSquareMatrix(1:5)
#' IsSquareMatrix(matrix(1:10, nrow = 2))
#' @export
IsSquareMatrix <- function(A) {
  UseMethod("IsSquareMatrix")
}

#' @rdname IsSquareMatrix
#' @export
IsSquareMatrix.default <- function(A) {
  if (IsMatrix(A)) {
    return(
      dim(A)[1] == dim(A)[2]
    )
  }
  return(FALSE)
}

#' @rdname IsSquareMatrix
#' @export
IsSquareMatrix.yac_symbol <- function(A) {
  out <- Ryacas::y_fn(A, "IsSquareMatrix")
  if (out$yacas_cmd == "True") {
    return(TRUE)
  }
  return(FALSE)
}
