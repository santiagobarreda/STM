
#' Estimate the likelihood of a given set of ffs
#'
#' This function estimates the likelihood of a given set of formant frequencies
#' given a set of category means and covariance matrices. It returns the mean,
#' standard deviation, and density of the likelihood.
#'
#' @param ffs A vector of log-transformed formant frequencies
#' @param means A vector of category means
#' @param covariance A matrix of category covariance
#' @param precision A matrix of category precision
#' @param log_det The log determinant of the category covariance matrix.
#' @return A named vector of the likelihood mean, likelihood standard
#' deviation, and peak log likelihood density.
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' nd = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' template = create_template(nd[, 1:3], nd$vowels)
#' ffs = log(as.numeric(d[1, c("f1", "f2", "f3")]))
#' estimate_likelihood(ffs, template$means["ae", ], template$covariance[["ae"]])
#' }
#' @export

estimate_likelihood = function(ffs, means, covariance, precision = NULL,
                               log_det = NULL){
  if (is.null(precision) || is.null(log_det)) {
    chol_factor = tryCatch(
      chol(covariance),
      error = function(e) {
        stop("Covariance matrix must be positive definite.", call. = FALSE)
      }
    )
    precision = chol2inv(chol_factor)
    log_det = 2 * sum(log(diag(chol_factor)))
  }

  ffs = as.numeric(unlist(ffs))
  means = as.numeric(means)
  intercept_template = sum((ffs - means) %*% precision)
  slope_template =   -sum(precision)
  likelihood_mu = (-intercept_template/slope_template)
  likelihood_sd = sqrt(-1/slope_template)
  likelihood_density = dmvnorm_fast(ffs-likelihood_mu, means, log=TRUE,
                                    precision = precision, log_det = log_det)
  output = c(likelihood_mu=likelihood_mu, likelihood_sd=likelihood_sd,
             likelihood_density=likelihood_density)
  return (output)
}
