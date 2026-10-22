#' Normalize formant data
#'
#' This function normalizes formant data using the least-squares method
#' described in Barreda and Nearey (2018). This method is robust in the presence
#' of to missing and unbalanced data. Log-mean normalized data and speaker
#' psi estimates are returned.
#'
#' Formants may be given in Hz or already log-transformed (data whose maximum
#' is 20 or less are assumed to be logged). Values of zero are treated as
#' missing.
#'
#' @param formants A matrix or data frame of formant frequencies
#' @param speakers A vector of speaker IDs
#' @param vowels A vector of vowel IDs
#' @return A data frame with the normalized log formant frequencies (columns
#' named \code{formants.<name>}), \code{vowels}, \code{speakers}, and
#' \code{gbar}, the speaker's estimated log-mean formant frequency (psi).
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' out = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' head(out)
#' }
#' @export

normalize = function(formants, speakers, vowels) {
  normalize_impute(formants, speakers, vowels, normalize = TRUE, impute = FALSE)
}
