#' Sliding Template Model
#'
#' Returns posterior probabilities for each category based on the template and
#' token acoustic properties.
#'
#' @param formants A vector of normalized formant frequencies, or a matrix or
#' data frame of formant vectors. Each row is a different observation, each
#' column is a formant measurement.
#' @param template A template object
#' @param vowel_priors A vector of prior probabilities for each vowel category.
#' If named, names must match \code{template$classes}; otherwise values must be
#' in the order of \code{template$classes}.
#' @param correctOUflow A boolean indicating whether to correct for underflow
#' and overflow using the correctOUflow() function.
#' @return An \code{STM_posteriors} object containing a matrix of posterior
#' probabilities, one row for each token.
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' nd = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' template = create_template(nd[, 1:3], nd$vowels)
#' post = STM(nd[1:10, 1:3], template)
#' post
#' apply(post, 1, which.max)
#' }
#' @export

STM = function(formants, template, vowel_priors = NULL, correctOUflow=TRUE) {
  if (is.data.frame(formants)) formants = as.matrix(formants)
  if (is.vector(formants)) formants = matrix(formants, nrow = 1)
  vowel_priors = check_vowel_priors(vowel_priors, template$classes)

  n_samples = nrow(formants)
  n_classes = nrow(template$means)
  log_vowel_density = matrix(0, n_samples, n_classes)

  for (j in 1:n_classes){
    log_vowel_density[, j] =  dmvnorm_fast (
      formants, template$means[j,], template$covariance[[j]], log = TRUE,
      precision = template$precision[[j]], log_det = template$log_det[[j]])
  }

  row_max = log_vowel_density[cbind(seq_len(n_samples), max.col(log_vowel_density))]
  log_vowel_density = log_vowel_density - row_max

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
