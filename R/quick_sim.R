#' Quick Simulation for Padtwins Function
#'
#' Runs a quick simulation on a padtwins function for a given number of iterations. It records whether the function runs without error, whether matrix P is invertible, and whether matrix A is successfully diagonalized.
#'
#' @param padtwins_func A padtwins function to be simulated.
#' @param iterations Number of iterations for the simulation (default: 100).
#' @return An object of class 'quick_sim_result' containing three logical vectors: run_without_error, p_invertible, and a_diagonalized.
#' @export
#' @examples
#' set.seed(102)
#' A <- matrix(c(1, 2, -2, 1), nrow = 2, ncol = 2)
#' sim_result <- quick_sim(function() padtwins(A), iterations = 100)
#' summary(sim_result)
#' plot(sim_result)
quick_sim <- function(padtwins_func, iterations = 100) {
  run_without_error <- logical(iterations)
  p_invertible <- logical(iterations)
  a_diagonalized <- logical(iterations)

  for (i in 1:iterations) {
    tryCatch({
      result <- padtwins_func()
      run_without_error[i] <- TRUE
      p_invertible[i] <- det(result$P) != 0
      a_diagonalized[i] <- all(result$transformed_A == result$D)
    }, error = function(e) {
      run_without_error[i] <- FALSE
    })
  }

  # Return results
  result <- list(run_without_error = run_without_error,
                 p_invertible = p_invertible,
                 a_diagonalized = a_diagonalized)

  class(result) <- "quick_sim_result"
  return(result)
}

#' Summary Method for Quick Simulation Results
#'
#' Generates a summary of the quick simulation results, showing the proportion of successful runs, invertibility of matrix P, and success in diagonalizing matrix A.
#'
#' @param object An object of class 'quick_sim_result'.
#' @param ... Additional arguments affecting the summary produced.
#' @return A printed summary of simulation results.
#' @export
#' @examples
#' set.seed(102)
#' A <- matrix(c(1, 2, -2, 1), nrow = 2, ncol = 2)
#' sim_result <- quick_sim(function() padtwins(A), iterations = 100)
#' summary(sim_result)
summary.quick_sim_result <- function(object, ...) {
  results <- sapply(object, function(x) sum(x) / length(x))
  cat("Summary of Quick Simulation Results:\n")
  print(results)
}

#' Plot Method for Quick Simulation Results
#'
#' Plots the quick simulation results, showing the proportion of successful runs, invertibility of matrix P, and success in diagonalizing matrix A in a bar chart.
#'
#' @param x An object of class 'quick_sim_result'.
#' @param ... Additional arguments affecting the plot produced.
#' @return A bar plot of the simulation results.
#' @export
#' @examples
#' set.seed(102)
#' A <- matrix(c(1, 2, -2, 1), nrow = 2, ncol = 2)
#' sim_result <- quick_sim(function() padtwins(A), iterations = 100)
#' plot(sim_result)
plot.quick_sim_result <- function(x, ...) {
  graphics::barplot(sapply(x, mean),
          names.arg = c("Run Without Error", "P Invertible", "A Diagonalized"),
          ylab = "Proportion",
          col = c("blue", "red", "green"),
          ...)
}
