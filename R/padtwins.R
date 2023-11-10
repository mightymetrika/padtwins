#' Padtwins Function
#'
#' This is a generic function for the padtwins package. It takes either a matrix or a function to generate a matrix and computes a specific matrix transformation.
#'
#' @param A A matrix or a function to generate a matrix. If a matrix is provided, it should be square. If a function is provided, it should return a square matrix.
#' @param generate_P A function to generate matrix 'P' based on the dimensions of 'A'. Defaults to a function that generates a matrix with binomially distributed numbers.
#' @param generate_D A function to generate a diagonal matrix 'D'. Defaults to a function that generates a vector with geometrically distributed numbers.
#' @param ... Additional arguments to be passed to specific methods.
#' @return An object of class 'padtwins' containing the results of the matrix transformation.
#' @export
#' @examples
#' # Using a predefined matrix
#' set.seed(102)
#' A <- matrix(c(1, 2, -2, 1), nrow = 2, ncol = 2)
#' result <- padtwins(A)
#'
#' # Using a function to generate matrix A
#' set.seed(110)
#' generate_A <- function() matrix(rhyper(4, 4, 5, 3), nrow = 2, ncol = 2)
#' padt <- padtwins(generate_A)
padtwins <- function(A,
                     generate_P = function(A) matrix(stats::rbinom(n = nrow(A) * ncol(A), size = 1, prob = 0.5), nrow = nrow(A)),
                     generate_D = function(A) c(stats::rgeom(n = nrow(A), prob = 0.5)),
                     ...) {
  UseMethod("padtwins", A)
}

#' Padtwins Method for Matrix Input
#'
#' This method of the padtwins function handles the scenario where the input 'A' is a predefined matrix. It performs specific matrix transformations based on matrices 'P' and 'D' generated using provided anonymous functions or default random generators.
#'
#' @param A A predefined square matrix.
#' @param generate_P A function to generate matrix 'P'. Defaults to a function that generates a matrix with binomially distributed numbers.
#' @param generate_D A function to generate a diagonal matrix 'D'. Defaults to a function that generates a vector with geometrically distributed numbers.
#' @param ... Additional arguments to be passed to specific methods.
#' @return An object of class 'padtwins' containing the results of the matrix transformation.
#' @export
padtwins.matrix <- function(A,
                            generate_P = function(A) matrix(stats::rbinom(n = nrow(A) * ncol(A), size = 1, prob = 0.5), nrow = nrow(A)),
                            generate_D = function(A) c(stats::rgeom(n = nrow(A), prob = 0.5)),
                            ...) {
  # Ensure A is a square matrix
  if (nrow(A) != ncol(A)) {
    stop("Matrix A must be square.")
  }

  # Generate matrices P and D
  P <- generate_P(A)
  D <- diag(generate_D(A))

  # Check if P is invertible
  if (det(P) == 0) {
    stop("Matrix P is not invertible.")
  }

  # Compute P^-1 * A * P
  transformed_A <- solve(P) %*% A %*% P

  # Check if transformed_A is equal to D
  is_equal <- all(transformed_A == D)

  # Get return object
  out <- list(equal = is_equal, P = P, D = D, A = A,transformed_A = transformed_A)

  class(out) <- "padtwins"

  return(out)
}

#' Padtwins Method for Function Input
#'
#' This method of the padtwins function handles the scenario where the input 'A' is a function that generates a matrix. It allows dynamic generation of matrix 'A' and performs specific matrix transformations.
#'
#' @param A A function to generate matrix 'A'.
#' @param generate_P A function to generate matrix 'P' based on the dimensions of 'A'. Defaults to a function that generates a matrix with binomially distributed numbers.
#' @param generate_D A function to generate a diagonal matrix 'D'. Defaults to a function that generates a vector with geometrically distributed numbers.
#' @param ... Additional arguments to be passed to specific methods.
#' @return An object of class 'padtwins' containing the results of the matrix transformation.
#' @export
padtwins.function <- function(A,
                              generate_P = function(A) matrix(stats::rbinom(n = nrow(A) * ncol(A), size = 1, prob = 0.5), nrow = nrow(A)),
                              generate_D = function(A) c(stats::rgeom(n = nrow(A), prob = 0.5)),
                              ...) {
  # Generate matrix A
  A <- A()

  # Ensure A is a square matrix
  if (nrow(A) != ncol(A)) {
    stop("Matrix A must be square.")
  }

  # Generate matrices P and D
  P <- generate_P(A)
  D <- diag(generate_D(A))

  # Check if P is invertible
  if (det(P) == 0) {
    stop("Matrix P is not invertible.")
  }

  # Compute P^-1 * A * P
  transformed_A <- solve(P) %*% A %*% P

  # Check if transformed_A is equal to D
  is_equal <- all(transformed_A == D)

  # Get return object
  out <- list(equal = is_equal, P = P, D = D, A = A,transformed_A = transformed_A)

  class(out) <- "padtwins"

  return(out)
}
