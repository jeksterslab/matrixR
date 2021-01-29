#' Dot Product
#'
#' Returns the dot product of two vectors.
#'
#' The dot product is the sum of the products of corresponding entries
#' of two vectors with the same length.
#'
#' @return Numeric.
#' @author Ivan Jacob Agaloos Pesigan
#' @family operation functions
#' @keywords operation
#' @param u Numeric vector of length `m`.
#' @param v Numeric vector of length `m`.
#' @param ... ...
#' @examples
#' Dot(c(1, 3, -5), c(4, -2, -1))
#' @references
#'   [Wikipedia: Dot product](https://en.wikipedia.org/wiki/Dot_product)
#' @export
Dot <- function(u, v, ...) {
  UseMethod("Dot")
}

#' @rdname Dot
#' @export
Dot.default <- function(u, v, ...) {
  u <- as.vector(u)
  v <- as.vector(v)
  if (length(u) != length(v)) {
    stop(
      "Inputs have unequal lengths."
    )
  }
  if (isFALSE(is.numeric(u)) & isFALSE(is.numeric(v))) {
    stop(
      "Inputs should be numeric."
    )
  }
  return(
    sum(
      u * v
    )
  )
}

#' @rdname Dot
#' @param str Logical.
#'   If `TRUE`, the function returns a character string.
#'   If `FALSE`, the function returns an `R` expression.
#' @export
Dot.yac_symbol <- function(u, v, str = TRUE, ...) {
  expr <- paste(u, ".", v)
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

#' Dot Product
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @family operation functions
#' @keywords operation
#' @examples
#' c(1, 3, -5) %.% c(4, -2, -1)
#' @inherit Dot description details return references
#' @inheritParams Dot
#' @export
"%.%" <- function(u, v) {
  return(
    Dot.default(u, v)
  )
}
