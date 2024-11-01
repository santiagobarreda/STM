#' Multivariate Normal Density
#'
#' Calculate the density of a multivariate normal distribution.
#'
#' @param features A matrix or vector of observations.
#' @param means A vector of means.
#' @param covariance A matrix of covariance.
#' @param log A boolean indicating whether to return the log density.
#' @return A scalar, the density of the multivariate normal distribution.
#' @examples
#' # TBD
#' @export
#'

dmvnorm_fast = function(features, means, covariance, log = FALSE) {

  if (is.data.frame(features)) as.matrix(features)
  if (is.vector(features)) features = matrix(features, nrow = 1)

  d = ncol(features)

  # Log determinant of covariance from the Cholesky decomposition
  L = chol(covariance)
  log_det_sigma = 2 * sum(log(diag(L)))

  # Inverse of covariance using Cholesky decomposition
  precision = chol2inv(L)

  # Center the observations by subtracting the mean vector
  # and calculate the quadratic form
  centered = as.matrix(sweep(features, 2,  means))
  quad_form = rowSums((centered %*% precision) * centered)

  # Compute log density
  log_density = -0.5 * (d * log(2 * pi) + log_det_sigma + quad_form)

  if (!log) return(exp(log_density))

  return (log_density)
}



