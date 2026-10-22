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
#' \dontrun{
#' x = seq(-3, 3, length.out = 200)
#' y = dnorm(x)
#' integrate_numerical(x, y)
#' }
#' @export


integrate_numerical = function(x, y) {
  n = length(x)
  if (n < 2 || length(y) != n)
    stop("x and y must have the same length, with at least two points.", call. = FALSE)
  integral = abs(sum((x[2:n] - x[1:(n-1)]) * (y[2:n] + y[1:(n-1)])) / 2)

  return(integral)
}

