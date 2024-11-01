
#' Estimate the likelihood of a given set of ffs
#'
#' This function estimates the likelihood of a given set of formant frequencies
#' given a set of category means and covariance matrices. It returns the mean,
#' standard deviation, and density of the likelihood.
#'
#' @param ffs A vector of formant frequencies
#' @param means A vector of category means
#' @param covariance A matrix of category covariance
#' @param precision A matrix of category precision
#' @return A list of the likelihood mean, likelihood standard deviation, and likelihood density.
#' @examples
#' # TBD
#' @export

estimate_likelihood = function(ffs, means, covariance, precision = solve(covariance)){
  intercept_template = sum(unlist(ffs - means) %*% precision)
  slope_template =   -sum(precision)
  likelihood_mu = (-intercept_template/slope_template)
  likelihood_sd = sqrt(-1/slope_template)
  likelihood_density = dmvnorm_fast(ffs-likelihood_mu, means, covariance, log=TRUE)
  output = c(likelihood_mu=likelihood_mu, likelihood_sd=likelihood_sd,
             likelihood_density=likelihood_density)
  return (output)
}
