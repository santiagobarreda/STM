#' Normalize formant data
#'
#' This function normalizes formant data using the least-squares method
#' described in Barreda and Nearey (2018). This method is robust in the presence
#' of to missing and unbalanced data. Log-mean normalized data and speaker
#' psi estimates are returned.
#'
#' @param formants A matrix of formant frequencies
#' @param speakers A vector of speaker IDs
#' @param vowels A vector of vowel IDs
#' @return A matrix of normalized formant data
#' @examples
#' #examples
#' # TBD
#' @export

normalize = function(formants, speakers, vowels) {
  normalize_impute(formants, speakers, vowels, normalize = TRUE, impute = FALSE)
}





