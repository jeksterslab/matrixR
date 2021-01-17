#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Square?
#'
#' @description Checks if a matrix is square \eqn{\left( \mathbf{A}_{m \times m} \right)} .
#'
#' @param A Numeric matrix.
#' @param chk.num Logical.
#'   Check if the input matrix is numeric.
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
#' is_sqr(B)
#' # Returns FALSE
#' is_sqr(A)
#' @references
#'   [Wikipedia: Square matrix](https://en.wikipedia.org/wiki/Square_matrix)
#' @export
is_sqr <- function(A,
                   chk.num = TRUE) {
  if (chk.num) {
    if (!is.numeric(A)) {
      stop(
        "`Input should be numeric."
      )
    }
  }
  if (!is.matrix(A)) {
    stop(
      "Input should be of class `matrix`."
    )
  }
  if (dim(A)[1] == dim(A)[2]) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Symmetric?
#'
#' @description Checks if a matrix is symmetric \eqn{\left( \mathbf{A} = \mathbf{A}^{\mathsf{T}} \right)}.
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
#' is_sym(Sigma)
#' @export
is_sym <- function(A,
                   chk.num = TRUE) {
  if (is_sqr(A, chk.num)) {
    return(
      sum(A == t(A)) == ((dim(A)[1])^2)
    )
  } else {
    return(FALSE)
  }
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Positive Definite?
#'
#' @description Checks if eigenvalues in a square matrix are positive.
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
#' is_posdef(Sigma)
#' @references
#'   [Wikipedia: Definiteness of a Matrix](https://en.wikipedia.org/wiki/Definiteness_of_a_matrix)
#' @export
is_posdef <- function(A,
                      tol = 1e-8) {
  if (is_sym(A)) {
    if (any(eigen(A, only.values = TRUE)$values < tol)) {
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
#' @description Checks if a square matrix is invertible.
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
#' is_inv(Sigma)
#' @references
#'   [Wikipedia: Invertible Matrix](https://en.wikipedia.org/wiki/Invertible_matrix)
#'
#'   [Wikipedia: Singular Matrix](https://en.wikipedia.org/wiki/Singular_matrix)
#' @export
is_inv <- function(A,
                   tol = 1e-8) {
  if (is_sqr(A)) {
    if (det(A) < tol) {
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
#' @description Checks if a square matrix is noninvertible or singular.
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
#' is_sing(Sigma)
#' @export
is_sing <- function(A,
                    tol = 1e-8) {
  return(
    !is_inv(
      A,
      tol = tol
    )
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Diagonal?
#'
#' @description Checks if off-diagonal elements of a square matrix are all zeroes.
#'
#' @details The off-diagonal elements are added and compared to a tolerance value.
#'   If the sum is less than or equal to the tolerance value,
#'   all the elements are assumed to be zeroes.
#'
#' @inheritParams is_posdef
#' @examples
#' I <- diag(3)
#' I
#' is_diag(I)
#' @references
#' [Wikipedia: Diagonal matrix](https://en.wikipedia.org/wiki/Diagonal_matrix)
#' @export
is_diag <- function(A,
                    tol = 1e-8) {
  if (!is_sqr(A)) {
    return(FALSE)
  } else {
    diag(A) <- rep(
      x = 0,
      length = dim(A)[1]
    )
    if (sum(as.vector(A)) < tol) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

# is_idempot <- function(A) {
#  if (!is.numeric(A)) {
#    stop(
#      "`Input should be numeric."
#    )
#  }
#  if (!is.matrix(A)) {
#    stop(
#      "Input should be of class `matrix`."
#    )
#  }
#  return(
#    all(crossprod(A) == A)
#  )
# }
