#' Dot Product of Tensors
#'
#' Returns the dot product (or inner product) of two tensors.
#'
#' This is a wrapper to the `%*%` infix operator and
#' a compatibility layer to the `Yacas` `Dot` command.
#'
#' If any of the arguments is a scalar,
#' the function performs scalar multiplication.
#'
#' @return Numeric.
#' @author Ivan Jacob Agaloos Pesigan
#' @family operation functions
#' @keywords operation
#' @param t1 Tensor. Vector or matrix.
#' @param t2 Tensor. Vector or matrix.
#' @param ... ...
#' @examples
#' Dot(c(1, 2), 3)
#' Dot(2, c(3, 4))
#' Dot(c(1, 2), c(3, 4))
#' Dot(rbind(c(1, 2), c(3, 4)), c(5, 6))
#' Dot(c(5, 6), rbind(c(1, 2), c(3, 4)))
#' Dot(rbind(c(1, 2), c(3, 4)), rbind(c(5, 6), c(7, 8)))
#' @export
Dot <- function(t1,
                t2,
                ...) {
  UseMethod("Dot")
}

#' @rdname Dot
#' @export
Dot.default <- function(t1,
                        t2,
                        ...) {
  # Scalar multiplication ----------------------------------------------
  if (IsScalar.default(t1)) {
    t1 <- as.vector(t1)
  }
  if (IsScalar.default(t2)) {
    t2 <- as.vector(t2)
  }
  if (IsScalar.default(t1) | IsScalar.default(t2)) {
    return(
      t1 * t2
    )
  }
  #---------------------------------------------------------------------
  # Matrix multiplication ----------------------------------------------
  return(
    t1 %*% t2
  )
  #---------------------------------------------------------------------
}

#' @rdname Dot
#' @param str Logical.
#'   If `TRUE`, the function returns a character string.
#'   If `FALSE`, the function returns an `R` expression.
#' @export
Dot.yac_symbol <- function(t1,
                           t2,
                           str = TRUE,
                           ...) {
  expr <- paste0(
    "Dot(",
      t1,
      ",",
      t2,
    ")"
  )
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
"%.%" <- function(t1, t2) {
  return(
    Dot.default(t1, t2)
  )
}
