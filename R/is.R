#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Square?
#'
#' @description Checks if the dimensions of a matrix are identical.
#'
#' @details
#'
#'   \deqn{
#'     \left(
#'       \mathbf{A}_{m \times m}
#'     \right)
#'   }
#'
#' @family is functions
#' @keywords is
#' @param A Matrix.
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
#' is_sqr(A)
#' # Returns FALSE
#' is_sqr(B)
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
  return(
    dim(A)[1] == dim(A)[2]
  )
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Symmetric?
#'
#' @description Checks if the transpose of a matrix is identical to the original matrix.
#'
#' @details
#'
#'   \deqn{
#'     \left(
#'       \mathbf{A}
#'       =
#'       \mathbf{A}^{\mathsf{T}}
#'     \right)
#'   }
#'
#' @family is functions
#' @keywords is
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
  out <- FALSE
  if (is_sqr(A, chk.num)) {
    out <- sum(A == t(A)) == ((dim(A)[1])^2)
  } else {
    warning(
      "Input is not a symmetric matrix."
    )
  }
  return(out)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Positive Definite?
#'
#' @description Checks if eigenvalues in a square matrix are positive.
#'
#' @family is functions
#' @keywords is
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
  out <- FALSE
  if (is_sym(A)) {
    out <- !any(eigen(A, only.values = TRUE)$values < tol)
  } else {
    warning(
      "Input is not a symmetric matrix."
    )
  }
  return(out)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Invertible?
#'
#' @description Checks if a square matrix is invertible.
#'
#' @details An \eqn{n \times n} matrix \eqn{\mathbf{A}} is invertible,
#'   if there exists an \eqn{n \times n} matrix \eqn{\mathbf{A}}
#'   such that
#'
#'   \deqn{
#'     \mathbf{A} \mathbf{B}
#'     =
#'     \mathbf{B}
#'     \mathbf{A}
#'     =
#'     \mathbf{I}_{n}
#'   } .
#'
#' @family is functions
#' @keywords is
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
  out <- FALSE
  if (is_sqr(A)) {
    out <- !(det(A) < tol)
  } else {
    warning(
      "Input is not a square matrix."
    )
  }
  return(out)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Singular?
#'
#' @description Checks if a square matrix is noninvertible or singular.
#'
#' @details An \eqn{n \times n} matrix \eqn{\mathbf{A}} is singular
#'   if its determinant is zero.
#'
#' @family is functions
#' @keywords is
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
#' @family is functions
#' @keywords is
#' @inheritParams is_posdef
#' @examples
#' I <- diag(3)
#' I
#' is_diag(I)
#' @references
#'   [Wikipedia: Diagonal matrix](https://en.wikipedia.org/wiki/Diagonal_matrix)
#' @export
is_diag <- function(A,
                    tol = 1e-8) {
  out <- FALSE
  if (is_sqr(A)) {
    diag(A) <- rep(
      x = 0,
      length = dim(A)[1]
    )
    out <- sum(as.vector(A)) < tol
  } else {
    warning(
      "Input is not a square matrix."
    )
  }
  return(out)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Nilpotent?
#'
#' @description Checks if the input matrix is nilpotent.
#'
#' @details The square matrix \eqn{\mathbf{A}} is nilpotent
#'   if all the eigenvalues of \eqn{\mathbf{A}} is zero.
#'
#' @family is functions
#' @keywords is
#' @inheritParams is_posdef
#' @references
#'   [Wikipedia: Nilpotent matrix](https://en.wikipedia.org/wiki/Nilpotent_matrix)
#' @examples
#' A <- zeroes(3, 3)
#' A[1, ] <- c(0, 1, 1)
#' is_nilpot(A)
#' @export
is_nilpot <- function(A,
                      tol = 1e-8) {
  out <- FALSE
  if (is_sqr(A)) {
    out <- all(
      abs(
        eigen(A, only.values = TRUE)$values
      ) < tol
    )
  } else {
    warning(
      "Input is not a square matrix."
    )
  }
  return(out)
}

#' @author Ivan Jacob Agaloos Pesigan
#'
#' @title Is the Matrix Idempotent?
#'
#' @description Checks if the input matrix is idempotent.
#'
#' @details The square matrix \eqn{\mathbf{A}} is idempotent
#'   if \eqn{\mathbf{A}^2 = \mathbf{A}}
#'
#' @family is functions
#' @keywords is
#' @inheritParams is_posdef
#' @references
#'   [Wikipedia: Nilpotent matrix](https://en.wikipedia.org/wiki/Idempotent_matrix)
#' @examples
#' A <- matrix(
#'   data = c(
#'     1, 0, 0, 1
#'   ),
#'   ncol = 2
#' )
#' is_idempot(A)
#' @export
is_idempot <- function(A,
                       tol = 1e-8) {
  out <- FALSE
  if (is_sqr(A)) {
    out <- sum(abs(A %*% A - A)) < tol
  } else {
    warning(
      "Input is not a square matrix."
    )
  }
  return(out)
}
