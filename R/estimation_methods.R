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
  parameters = scalar_params(list(PSI_prior_mean=PSI_prior_mean, PSI_prior_sd=PSI_prior_sd,
                                  f0_hat_sd=f0_hat_sd,f0_hat_intercept=f0_hat_intercept,
                                  f0_hat_slope=f0_hat_slope))
  run_single_token(ffs, f0, template, "f0", parameters, parameters, vowel_priors,
                   type, correctOUflow, lite)
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
  parameters = scalar_params(list(PSI_prior_mean=PSI_prior_mean, PSI_prior_sd=PSI_prior_sd))
  run_single_token(ffs, f0, template, "fixed", parameters, parameters, vowel_priors,
                   type, correctOUflow, lite)
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
  run_single_token(ffs, f0, template, "none", NULL, NA, vowel_priors,
                   type, correctOUflow, lite)
}


# Run one token through the vectorized engine.
#' @noRd

run_single_token = function(ffs, f0, template, prior, params, parameters,
                            vowel_priors, type, correctOUflow, lite){
  x = matrix(as.numeric(unlist(ffs)), nrow = 1)
  batch = xstm_batch(x, f0, template, prior, params, vowel_priors, type,
                     correctOUflow)
  if (lite)
    return(list(psi = batch$posterior_mu[1, ],
                posterior = batch$posterior_probability[1, ]))
  results = batch_results(batch, x, f0, template, parameters, vowel_priors,
                          lite = FALSE, stimuli = list(c(ffs, f0 = f0)))
  return(results$outputs[[1]])
}
