#' Test for a Scalar
#'
#' [IsScalar()] returns `TRUE` if `k` is a scalar,
#' and `FALSE` otherwise.
#'
#' A scalar is any real number that can be measured
#' using a single real number.
#' A scalar is typically denoted by \eqn{k}.
#'
#' `k` is considered to be a scalar
#' if it can be coerced to a vector
#' and `length(k) == 1` returns `TRUE`.
#'
#' @return Logical.
#' @author Ivan Jacob Agaloos Pesigan
#' @family predicate functions
#' @keywords predicate
#' @param k Input.
#' @examples
#' # TRUE
#' IsScalar(1)
#' IsScalar(as.matrix(1))
#'
#' # FALSE
#' IsScalar(c(1, 2, 3))
#' IsScalar(as.matrix(c(1, 2, 3)))
#' @export
IsScalar <- function(k) {
  UseMethod("IsScalar")
}

#' @rdname IsScalar
#' @export
IsScalar.default <- function(k) {
  if (is.vector(k) | is.matrix(k)) {
    return(
      length(as.vector(k)) == 1
    )
  }
  return(FALSE)
}

#' @rdname IsScalar
#' @export
IsScalar.yac_symbol <- function(k) {
  out <- Ryacas::y_fn(k, "IsScalar")
  if (out$yacas_cmd == "True") {
    return(TRUE)
  }
  return(FALSE)
}
