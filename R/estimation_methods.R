#' Method 6 psi estimation
#'
#' This function estimates the psi value using the method 6 from Nearey and Assmann (2007).
#' It also returns the log density of the estimated psi value.
#'
#' @param ffs A vector of formant frequencies
#' @param f0 A scalar of fundamental frequency
#' @param template A template object containing category means and covariance information
#' @param PSI_prior_mean A scalar of the prior mean for psi
#' @param PSI_prior_sd A scalar of the prior standard deviation for psi
#' @param f0_hat_sd A scalar of the standard deviation for f0 given psi
#' @param f0_hat_intercept A scalar of the intercept for f0 given psi
#' @param f0_hat_slope A scalar of the slope for f0 given psi
#' @param vowel_priors A vector of prior probabilities for each category
#' @param correctOUflow A boolean indicating whether to correct for underflow
#' @param type A string indicating the type of method to use for estimating psi
#' @param ... additional parameters
#'
#' @return A scalar of the estimated psi value, and optionally the log density at that location.
#' @examples
#' #examples
#'# TBD
#' @export

method6 = function (ffs, f0, template, PSI_prior_mean=7.233,
                    PSI_prior_sd = 0.1284, f0_hat_sd=0.1327,
                    f0_hat_intercept=-10.32, f0_hat_slope=2.145,
                    vowel_priors=vowel_priors,
                    correctOUflow=TRUE, type="BSTM", ...){

  n_classes = nrow(template$means)
  likelihoods = t(sapply (1:n_classes, function(j){
    estimate_likelihood(ffs, template$means[j,],
                        template$covariance[[j]], template$precision[[j]])
  }))
  rownames (likelihoods) = rownames(template$means)

  priors = estimate_f0_psi_prior (f0)
  priors = matrix(priors,n_classes,3,byrow=TRUE)
  colnames(priors) = c('prior_mu', 'prior_sd', 'prior_density')

  if (!is.null(vowel_priors))
    priors$prior_density = priors$prior_density + log(vowel_priors)

  posterior = find_posterior(likelihoods, priors, type = type)
  if(correctOUflow) posterior[,4] = correctOUflow_internal(posterior[,4])

  stimulus = c(ffs, f0)
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
#' @param ffs A vector of formant frequencies
#' @param f0 A scalar of fundamental frequency
#' @param template A template object containing category means and covariance information
#' @param PSI_prior_mean A scalar of the prior mean for psi
#' @param PSI_prior_sd A scalar of the prior standard deviation for psi
#' @param vowel_priors A vector of prior probabilities for each category
#' @param correctOUflow A boolean indicating whether to correct for underflow
#' @param type A string indicating the type of method to use for estimating psi
#' @param ... additional parameters
#'
#' @return A scalar of the estimated psi value, and optionally the log density at that location.
#' @examples
#' #examples
#'# TBD
#' @export


method3 = function (ffs, f0 = NA, template, PSI_prior_mean=7.233,
                    PSI_prior_sd = 0.1284,vowel_priors=NULL,
                    correctOUflow=TRUE,type="BSTM",
                    ...){

  n_classes = nrow(template$means)
  likelihoods = t(sapply (1:n_classes, function(j){
    estimate_likelihood(ffs, template$means[j,],
                        template$covariance[[j]], template$precision[[j]])
  }))
  rownames (likelihoods) = rownames(template$means)

  priors = matrix(c(PSI_prior_mean,PSI_prior_sd,
                    log(1/sqrt(PSI_prior_sd^2 * pi * 2))),
                  n_classes,3,byrow=TRUE)
  colnames(priors) = c('prior_mu', 'prior_sd', 'prior_density')

  if (!is.null(vowel_priors))
    priors$prior_density = priors$prior_density + log(vowel_priors)

  posterior = find_posterior(likelihoods, priors, type = type)
  if(correctOUflow) posterior[,4] = correctOUflow_internal(posterior[,4])

  stimulus = c(ffs, f0 = f0)
  parameters = c(PSI_prior_mean=PSI_prior_mean, PSI_prior_sd=PSI_prior_sd)
  output = STM_output(cbind(posterior, likelihoods, priors), stimulus,
                      template, parameters,vowel_priors)

  return(output)
}


#' Method 2 psi estimation
#'
#' This function estimates the psi value using the method 3 from Nearey and Assmann (2007).
#' It also returns the log density of the estimated psi value.
#'
#' @param ffs A vector of formant frequencies
#' @param f0 A scalar of fundamental frequency
#' @param template A template object containing category means and covariance information
#' @param vowel_priors A vector of prior probabilities for each category
#' @param correctOUflow A boolean indicating whether to correct for underflow
#' @param type A string indicating the type of method to use for estimating psi
#' @param ... additional parameters
#' @return A scalar of the estimated psi value, and optionally the log density at that location.
#' @examples
#' #examples
#'# TBD
#' @export


method2 = function (ffs, f0=NA, template, vowel_priors=NULL, correctOUflow=TRUE,type="BSTM", ...){

  n_classes = nrow(template$means)
  likelihoods = t(sapply (1:n_classes, function(j){
    estimate_likelihood(ffs, template$means[j,],
                        template$covariance[[j]], template$precision[[j]])
  }))
  rownames (likelihoods) = rownames(template$means)
  posterior = cbind(likelihoods,0,0)

  priors = matrix (NA, n_classes, 3)
  colnames(priors) = c('prior_mu', 'prior_sd', 'prior_density')

  if (!is.null(vowel_priors)){
    posterior[,3] = posterior[,3] + log(vowel_priors)
    priors$prior_density = log(vowel_priors)
  }

  posterior = scale_posterior(posterior, type=type)
  colnames(posterior) = c('posterior_mu', 'posterior_sd', 'posterior_density',
                          'posterior_probability', 'rounded_pp')

  if(correctOUflow) posterior[,4] = correctOUflow_internal(posterior[,4])


  output = cbind(posterior, likelihoods, priors)

  stimulus = c(ffs, f0 = NA)
  parameters = NA
  output = STM_output(cbind(posterior, likelihoods, priors), stimulus,
                      template, parameters,vowel_priors)

  return(output)
}

