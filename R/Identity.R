#' Identity Matrix \eqn{\mathbf{I}}
#'
#' Generates an identity matrix.
#'
#' The \eqn{m \times m} identity matrix \eqn{\mathbf{I}} is given by
#'   \deqn{
#'     \mathbf{I}_{m}
#'     =
#'     \mathrm{diag} \left(1, 1, \cdots, 1 \right) .
#'   }
#'
#' @return Numeric matrix.
#' @author Ivan Jacob Agaloos Pesigan
#' @family constructor functions
#' @keywords constructor
#' @param m Integer. Size.
#' @examples
#' Identity(3)
#' @references
#'   [Wikipedia: Identity matrix](https://en.wikipedia.org/wiki/Identity_matrix)
#' @export
Identity <- function(m) {
  return(
    diag(m)
  )
}

#' Identity Matrix \eqn{\mathbf{I}} from Dimensions of Another Square Matrix
#'
#' Generates an identity matrix
#' using the dimensions of an \eqn{m \times m} matrix \eqn{\mathbf{A}}.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family constructor functions
#' @keywords constructor
#' @inheritParams IsMatrix
#' @inherit Identity details return references
#' @examples
#' IdentityFrom(matrix(1:9, ncol = 3))
#' @export
IdentityFrom <- function(A) {
  if (isFALSE(IsSquareMatrix.default(A))) {
    stop(
      "Input should be s square matrix."
    )
  }
  Identity <- Identity(dim(A)[1])
  colnames(Identity) <- colnames(A)
  rownames(Identity) <- rownames(A)
  return(Identity)
}
