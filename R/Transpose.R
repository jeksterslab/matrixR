#' Transpose of a Matrix
#'
#' Derives the tranpose of a matrix.
#'
#' The transpose of an \eqn{m \times n} matrix \eqn{\mathbf{A}}
#' is given by
#'   \deqn{
#'     \mathbf{A}_{i, j}^{\mathsf{T}} = \mathbf{A}_{j, i}
#'   }
#'
#' This is a wrapper around the `t` function
#' and a compatibility layer to the `Yacas` `Transpose` command.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family operation functions
#' @keywords operation
#' @inheritParams IsMatrix
#' @references
#'   [Wikipedia: Transpose](https://en.wikipedia.org/wiki/Transpose)
#' @examples
#' Transpose(matrix(data = c(1, 3, 2, 4), ncol = 2))
#' @export
Transpose <- function(A,
                      ...) {
  UseMethod("Transpose")
}

#' @rdname Transpose
#' @export
Transpose.default <- function(A,
                              ...) {
  return(
    t(A)
  )
}

#' @rdname Transpose
#' @inheritParams Dot.yac_symbol
#' @export
Transpose.yac_symbol <- function(A,
                                 str = TRUE,
                                 ...) {
  expr <- paste0("Transpose(", A, ")")
  if (str) {
    return(
      Ryacas::yac_str(expr)
    )
  } else {
    return(
      Ryacas::yac_expr(expr)
    )
  }
}
