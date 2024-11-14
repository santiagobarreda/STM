#' Sliding Template Model
#'
#' Returns posterior probabilities for each category based on the template and
#' token acoustic properties.
#'
#' @param formants A vector of formant frequencies, or a matrix or data frame of formant vectors. Each row
#' is a different observation, each column is a formant measurement.
#' @param template A template object
#' @param vowel_priors A vector of prior probabilities for each vowel category.
#' @param correctOUflow A boolean indicating whether to correct for underflow
#' and overflow using the correctOUflow() function.
#' @return A dataframe of posterior probabilities, one row for each token.
#' a list of dataframes.
#' @examples
#' # TBD
#' @export

STM = function(formants, template, vowel_priors = NULL, correctOUflow=TRUE) {
  n_samples = nrow(formants)
  n_classes = nrow(template$means)
  log_vowel_density = matrix(0, n_samples, n_classes)

  for (j in 1:n_classes){
    log_vowel_density[, j] =  dmvnorm_fast (
      formants, template$means[j,], template$covariance[[j]], log = TRUE)
  }

  log_vowel_density = log_vowel_density - apply(log_vowel_density, 1, function(x) mean(x))

  if (!is.null(vowel_priors))
    log_vowel_density = sweep(log_vowel_density, 2, log(vowel_priors), `+`)

  vowel_densities = exp(log_vowel_density)
  posterior_probabilities = vowel_densities / rowSums(vowel_densities)
  if(correctOUflow)
      posterior_probabilities = correctOUflow_internal(posterior_probabilities)

  colnames(posterior_probabilities) = rownames(template$means)
  posterior_probabilities = STM_posteriors(posterior_probabilities)
  return(posterior_probabilities)
}

