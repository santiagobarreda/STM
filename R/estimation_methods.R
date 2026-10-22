#' Method 6 psi estimation
#'
#' This function estimates the psi value using the method 6 from Nearey and Assmann (2007).
#' It also returns the log density of the estimated psi value.
#'
#' @param ffs A vector of log-transformed formant frequencies
#' @param f0 A scalar of log-transformed fundamental frequency
#' @param template A template object containing category means and covariance information
#' @param PSI_prior_mean A scalar of the prior mean for psi
#' @param PSI_prior_sd A scalar of the prior standard deviation for psi
#' @param f0_hat_sd A scalar of the standard deviation for f0 given psi
#' @param f0_hat_intercept A scalar of the intercept for f0 given psi
#' @param f0_hat_slope A scalar of the slope for f0 given psi
#' @param vowel_priors A vector of prior probabilities for each category
#' @param correctOUflow A boolean indicating whether to correct for underflow
#' @param type Either "BSTM" or "PSTM", indicating how posterior probabilities
#' are calculated.
#' @param lite A boolean indicating whether to return compact output for speed.
#' @param ... additional parameters
#'
#' @return An \code{STM_output} object or, if \code{lite = TRUE}, a list with
#' the psi estimate and posterior probability for each category.
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' nd = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' template = create_template(nd[, 1:3], nd$vowels)
#' method6(log(as.numeric(d[1, c("f1", "f2", "f3")])), f0 = log(d$f0[1]),
#'         template = template)
#' }
#' @export

method6 = function (ffs, f0, template, PSI_prior_mean=7.233,
                    PSI_prior_sd = 0.1284, f0_hat_sd=0.1327,
                    f0_hat_intercept=-10.32, f0_hat_slope=2.145,
                    vowel_priors=NULL,
                    correctOUflow=TRUE, type="BSTM", lite=FALSE, ...){

  type = match.arg(type, c("BSTM", "PSTM"))
  vowel_priors = check_vowel_priors(vowel_priors, template$classes)
  n_classes = nrow(template$means)
  likelihoods = template_likelihoods(ffs, template)

  priors_f0_adjusted = estimate_f0_psi_prior(f0, PSI_prior_mean=PSI_prior_mean,
                                              PSI_prior_sd=PSI_prior_sd,
                                              f0_hat_sd=f0_hat_sd,
                                              f0_hat_intercept=f0_hat_intercept,
                                              f0_hat_slope=f0_hat_slope)
  priors = matrix(priors_f0_adjusted, n_classes, 3, byrow=TRUE)
  colnames(priors) = c('prior_mu', 'prior_sd', 'prior_density')

  if (!is.null(vowel_priors))
    priors[, "prior_density"] = priors[, "prior_density"] + log(vowel_priors)

  posterior = find_posterior(likelihoods, priors, type = type)
  posterior = finalize_posterior(posterior, correctOUflow)

  if (lite) {
    return(list(psi = posterior[, "posterior_mu"],
                posterior = posterior[, "posterior_probability"]))
  }

  stimulus = c(ffs, f0 = f0)
  parameters = c(PSI_prior_mean=PSI_prior_mean, PSI_prior_sd=PSI_prior_sd,
                 f0_hat_sd=f0_hat_sd,f0_hat_intercept=f0_hat_intercept,
                 f0_hat_slope=f0_hat_slope)
  output = STM_output(cbind(posterior, likelihoods, priors), stimulus,
                      template, parameters,vowel_priors)
  return(output)
}


#' Method 3 psi estimation
#'
#' This function estimates the psi value using the method 3 from Nearey and Assmann (2007).
#' It also returns the log density of the estimated psi value.
#'
#' @param ffs A vector of log-transformed formant frequencies
#' @param f0 A scalar of log-transformed fundamental frequency (stored, but
#' not used in estimation)
#' @param template A template object containing category means and covariance information
#' @param PSI_prior_mean A scalar of the prior mean for psi
#' @param PSI_prior_sd A scalar of the prior standard deviation for psi
#' @param vowel_priors A vector of prior probabilities for each category
#' @param correctOUflow A boolean indicating whether to correct for underflow
#' @param type Either "BSTM" or "PSTM", indicating how posterior probabilities
#' are calculated.
#' @param lite A boolean indicating whether to return compact output for speed.
#' @param ... additional parameters
#'
#' @return An \code{STM_output} object or, if \code{lite = TRUE}, a list with
#' the psi estimate and posterior probability for each category.
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' nd = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' template = create_template(nd[, 1:3], nd$vowels)
#' method3(log(as.numeric(d[1, c("f1", "f2", "f3")])), template = template)
#' # or, through BSTM():
#' BSTM(log(d[1, c("f1", "f2", "f3")]), log(d$f0[1]), template, method = method3)
#' }
#' @export

method3 = function (ffs, f0 = NA, template, PSI_prior_mean=7.233,
                    PSI_prior_sd = 0.1284,vowel_priors=NULL,
                    correctOUflow=TRUE,type="BSTM", lite=FALSE,
                    ...){

  type = match.arg(type, c("BSTM", "PSTM"))
  vowel_priors = check_vowel_priors(vowel_priors, template$classes)
  n_classes = nrow(template$means)
  likelihoods = template_likelihoods(ffs, template)

  priors = matrix(c(PSI_prior_mean,PSI_prior_sd,
                    log(1/sqrt(PSI_prior_sd^2 * pi * 2))),
                  n_classes,3,byrow=TRUE)
  colnames(priors) = c('prior_mu', 'prior_sd', 'prior_density')

  if (!is.null(vowel_priors))
    priors[, "prior_density"] = priors[, "prior_density"] + log(vowel_priors)

  posterior = find_posterior(likelihoods, priors, type = type)
  posterior = finalize_posterior(posterior, correctOUflow)

  if (lite) {
    return(list(psi = posterior[, "posterior_mu"],
                posterior = posterior[, "posterior_probability"]))
  }

  stimulus = c(ffs, f0 = f0)
  parameters = c(PSI_prior_mean=PSI_prior_mean, PSI_prior_sd=PSI_prior_sd)
  output = STM_output(cbind(posterior, likelihoods, priors), stimulus,
                      template, parameters,vowel_priors)

  return(output)
}


#' Method 2 psi estimation
#'
#' This function estimates the psi value using the method 2 from Nearey and
#' Assmann (2007), which uses a uniform prior on psi.
#' It also returns the log density of the estimated psi value.
#'
#' @param ffs A vector of log-transformed formant frequencies
#' @param f0 A scalar of log-transformed fundamental frequency (stored, but
#' not used in estimation)
#' @param template A template object containing category means and covariance information
#' @param vowel_priors A vector of prior probabilities for each category
#' @param correctOUflow A boolean indicating whether to correct for underflow
#' @param type Either "BSTM" or "PSTM", indicating how posterior probabilities
#' are calculated.
#' @param lite A boolean indicating whether to return compact output for speed.
#' @param ... additional parameters
#' @return An \code{STM_output} object or, if \code{lite = TRUE}, a list with
#' the psi estimate and posterior probability for each category.
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' nd = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' template = create_template(nd[, 1:3], nd$vowels)
#' method2(log(as.numeric(d[1, c("f1", "f2", "f3")])), template = template)
#' }
#' @export

method2 = function (ffs, f0=NA, template, vowel_priors=NULL, correctOUflow=TRUE,
                    type="BSTM", lite=FALSE, ...){

  type = match.arg(type, c("BSTM", "PSTM"))
  vowel_priors = check_vowel_priors(vowel_priors, template$classes)
  n_classes = nrow(template$means)
  likelihoods = template_likelihoods(ffs, template)

  priors = matrix (NA_real_, n_classes, 3)
  colnames(priors) = c('prior_mu', 'prior_sd', 'prior_density')

  posterior = cbind(likelihoods, 0, 0)
  if (!is.null(vowel_priors)){
    posterior[,3] = posterior[,3] + log(vowel_priors)
    priors[, "prior_density"] = log(vowel_priors)
  }

  posterior = scale_posterior(posterior, type=type)
  colnames(posterior) = c('posterior_mu', 'posterior_sd', 'posterior_density',
                          'posterior_probability', 'rounded_pp')
  posterior = finalize_posterior(posterior, correctOUflow)

  if (lite) {
    return(list(psi = posterior[, "posterior_mu"],
                posterior = posterior[, "posterior_probability"]))
  }

  stimulus = c(ffs, f0 = f0)
  parameters = NA
  output = STM_output(cbind(posterior, likelihoods, priors), stimulus,
                      template, parameters,vowel_priors)

  return(output)
}
