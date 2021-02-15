#' Check Matrix Properties
#'
#' Checks matrix properties and stops if any test fails.
#' Returns the matrix if all tests succeed.
#'
#' @family predicate functions
#' @keywords predicate
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams IsMatrix
#' @param IsSquareMatrix Logical.
#'   Test if input is a square matrix.
#' @param IsSymmetric Logical.
#'   Test if input is a symmetric matrix.
#' @param ... ...
#' @export
MatrixCheck <- function(A,
                        IsSquareMatrix = FALSE,
                        IsSymmetric = FALSE,
                        ...) {
  UseMethod("MatrixCheck")
}

#' @rdname MatrixCheck
#' @inheritParams MatrixCheck
#' @export
MatrixCheck.default <- function(A,
                                IsSquareMatrix = FALSE,
                                IsSymmetric = FALSE,
                                ...) {
  if (IsSymmetric) {
    IsSquareMatrix <- FALSE
    stopifnot(
      IsSymmetric(
        A
      )
    )
  }
  if (IsSquareMatrix) {
    stopifnot(
      IsSquareMatrix(
        A
      )
    )
  }
  return(A)
}


#' @rdname MatrixCheck
#' @inheritParams MatrixCheck
#' @export
MatrixCheck.yac_symbol <- function(A,
                                   IsSquareMatrix = FALSE,
                                   IsSymmetric = FALSE,
                                   ...) {
  stopifnot(methods::is(A, "yac_symbol"))
  A <- Ryacas::ysym(
    Ryacas::yac_str(
      A$yacas_cmd
    )
  )
  stopifnot(
    A$is_mat
  )
  if (IsSymmetric) {
    IsSquareMatrix <- FALSE
    stopifnot(
      IsSymmetric(
        A
      )
    )
  }
  if (IsSquareMatrix) {
    stopifnot(
      IsSquareMatrix(
        A
      )
    )
  }
  return(A)
}
