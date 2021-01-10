#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Square?
#'
#' @param X Numeric matrix.
#' @examples
#' A <- matrix(
#'   data = 1:9,
#'   nrow = 3
#' )
#' dim(A)
#' B <- matrix(
#'   data = 1:10,
#'   ncol = 2
#' )
#' dim(B)
#' # Returns TRUE
#' is_sqr(X = B)
#' # Returns FALSE
#' is_sqr(X = A)
#' @references
#'   [Wikipedia: Square matrix](https://en.wikipedia.org/wiki/Square_matrix)
#' @export
is_sqr <- function(X) {
  if (!is.numeric(X)) {
    stop(
      "`X` should be numeric."
    )
  }
  if (!is.matrix(X)) {
    stop(
      "`X` should be a matrix."
    )
  }
  if (dim(X)[1] == dim(X)[2]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Symmetric?
#'
#' @inheritParams is_sqr
#' @examples
#' Sigma <- matrix(
#'   data = c(
#'     225, 112.50, 56.25,
#'     112.5, 225, 112.5,
#'     56.25, 112.50, 225
#'   ),
#'   ncol = 3
#' )
#' is_sym(X = Sigma)
#' @export
is_sym <- function(X) {
  if (is_sqr(X = X)) {
    return(sum(X == t(X)) == (nrow(X)^2))
  } else {
    return(FALSE)
  }
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Positive Definite?
#'
#' @inheritParams is_sym
#' @param tol Numeric.
#'   Tolerance.
#' @examples
#' Sigma <- matrix(
#'   data = c(
#'     225, 112.50, 56.25,
#'     112.5, 225, 112.5,
#'     56.25, 112.50, 225
#'   ),
#'   ncol = 3
#' )
#' is_posdef(X = Sigma)
#' @references
#'   [Wikipedia: Definiteness of a Matrix](https://en.wikipedia.org/wiki/Definiteness_of_a_matrix)
#' @export
is_posdef <- function(X,
                      tol = 1e-8) {
  if (is_sym(X = X)) {
    eigenvalues <- eigen(
      x = X,
      only.values = TRUE
    )$values
    p <- dim(X)[1]
    for (i in 1:p) {
      if (abs(eigenvalues[i] < tol)) {
        eigenvalues[i] <- 0
      }
    }
    if (any(eigenvalues <= 0)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Invertible?
#'
#' @inheritParams is_posdef
#' @examples
#' Sigma <- matrix(
#'   data = c(
#'     225, 112.50, 56.25,
#'     112.5, 225, 112.5,
#'     56.25, 112.50, 225
#'   ),
#'   ncol = 3
#' )
#' is_inv(X = Sigma)
#' @references
#'   [Wikipedia: Invertible Matrix](https://en.wikipedia.org/wiki/Invertible_matrix)
#'
#'   [Wikipedia: Singular Matrix](https://en.wikipedia.org/wiki/Singular_matrix)
#' @export
is_inv <- function(X,
                   tol = 1e-8) {
  if (is_sqr(X = X)) {
    if (det(X) < tol) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Singular?
#'
#' @inheritParams is_inv
#' @inherit is_inv references
#' @examples
#' Sigma <- matrix(
#'   data = c(
#'     225, 112.50, 56.25,
#'     112.5, 225, 112.5,
#'     56.25, 112.50, 225
#'   ),
#'   ncol = 3
#' )
#' is_sing(X = Sigma)
#' @export
is_sing <- function(X,
                    tol = 1e-8) {
  return(
    !is_inv(
      X = X,
      tol = tol
    )
  )
}
