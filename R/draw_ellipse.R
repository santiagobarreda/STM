
#' Draw an ellipse
#'
#' @param means A vector of length 2 with the x and y coordinates of the center of the ellipse
#' @param covariance A 2x2 matrix with the covariance matrix of the ellipse
#' @param n_points The number of points to use to draw the ellipse
#' @param show A boolean indicating whether to plot the ellipse
#' @param ... Additional arguments to pass to the lines function
#'
#' @examples
#' # TBD
#' @export


draw_ellipse = function(means, covariance, n_points = 100, show = TRUE,...) {
  eig <- eigen(covariance)
  angles <- seq(0, 2 * pi, length.out = n_points)
  unit_circle <- cbind(cos(angles), sin(angles))
  ellipse_points <- unit_circle %*% diag(sqrt(eig$values))
  ellipse_points <- ellipse_points %*% t(eig$vectors)
  ellipse_points <- sweep(ellipse_points, 2, unlist(means), '+')
  if (show) lines(ellipse_points, type = 'l', ...)
  invisible (ellipse_points)
}
