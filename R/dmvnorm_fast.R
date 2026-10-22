#' Multivariate Normal Density
#'
#' Calculate the density of a multivariate normal distribution.
#'
#' @param features A matrix or vector of observations.
#' @param means A vector of means.
#' @param covariance A matrix of covariance.
#' @param log A boolean indicating whether to return the log density.
#' @param precision Optional precomputed inverse of \code{covariance}.
#' @param log_det Optional precomputed log determinant of \code{covariance}.
#' If both \code{precision} and \code{log_det} are provided, \code{covariance}
#' is not used and no matrix decomposition is performed.
#' @return A vector of densities, one for each row of \code{features}.
#' @examples
#' \dontrun{
#' features = log(matrix(c(500, 1500, 2500), nrow = 1))
#' means = log(c(520, 1480, 2480))
#' covariance = diag(c(0.01, 0.005, 0.004))
#' dmvnorm_fast(features, means, covariance, log = TRUE)
#' }
#' @export
#'

dmvnorm_fast = function(features, means, covariance, log = FALSE,
                        precision = NULL, log_det = NULL) {

  if (is.data.frame(features)) features = as.matrix(features)
  if (is.vector(features)) features = matrix(features, nrow = 1)

  means = as.numeric(means)
  if (ncol(features) != length(means)) {
    stop("ncol(features) must equal length(means).", call. = FALSE)
  }

  d = ncol(features)

  if (is.null(precision) || is.null(log_det)) {
    # Log determinant and inverse of covariance from the Cholesky decomposition
    L = tryCatch(
      chol(covariance),
      error = function(e) {
        stop("Covariance matrix must be positive definite.", call. = FALSE)
      }
    )
    log_det = 2 * sum(log(diag(L)))
    precision = chol2inv(L)
  }

  # Center the observations by subtracting the mean vector
  # and calculate the quadratic form
  centered = features - matrix(means, nrow(features), d, byrow = TRUE)
  quad_form = rowSums((centered %*% precision) * centered)

  # Compute log density
  log_density = -0.5 * (d * log(2 * pi) + log_det + quad_form)

  if (!log) return(exp(log_density))

  return (log_density)
}
