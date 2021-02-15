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
#' @param IsNilpotent Logical.
#'   Test if input is a nilponent matrix.
#' @param ... ...
#' @export
MatrixCheck <- function(A,
                        IsSquareMatrix = FALSE,
                        IsSymmetric = FALSE,
                        IsNilpotent = FALSE,
                        ...) {
  UseMethod("MatrixCheck")
}

#' @rdname MatrixCheck
#' @inheritParams MatrixCheck
#' @export
MatrixCheck.default <- function(A,
                                IsSquareMatrix = FALSE,
                                IsSymmetric = FALSE,
                                IsNilpotent = FALSE,
                                ...) {
  if (IsSymmetric) {
    IsSquareMatrix <- FALSE
    stopifnot(
      IsSymmetric(
        A
      )
    )
  }
  if (IsNilpotent) {
    IsSquareMatrix <- FALSE
    stopifnot(
      IsNilpotent(
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
                                   IsNilpotent = FALSE,
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
  return(
    MatrixCheck.default(
      A = A,
      IsSquareMatrix = IsSquareMatrix,
      IsSymmetric = IsSymmetric,
      # not yet available in yacas
      IsNilpotent = FALSE
    )
  )
}
