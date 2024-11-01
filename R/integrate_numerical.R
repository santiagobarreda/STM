#' Numerical integration
#'
#' This function calculates the integral of a function using the trapezoidal
#' rule. The function iaccepts a set of x and y values and returns the
#' integral of the function.
#'
#' @param x A vector of x values
#' @param y A vector of y values
#' @return A scalar of the integral of the function.
#' @examples
#' # TBD
#' @export


integrate_numerical = function(x, y) {
  n = length(x)
  integral = abs(sum((x[2:n] - x[1:(n-1)]) * (y[2:n] + y[1:(n-1)])) / 2)

  return(integral)
}

