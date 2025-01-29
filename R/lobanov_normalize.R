
#' Lobanov normalization
#'
#' This function normalizes formant data using Lobanov normalization.
#' This method is not recommended for most linguistic research.
#' For more information on this, please see:
#'
#' Barreda, S. (2021). Perceptual validation of vowel normalization methods
#' for variationist research. Language Variation and Change, 33(1), 27-53.
#'
#' @param formants A matrix of formant data.
#' @param speakers A vector of speaker IDs.
#' @param vowels A vector of vowel IDs.
#' @param warning A boolean indicating whether to display a warning message.
#' @return A data frame of normalized formant data.
#' @examples
#' # TBD
#' @export
#' @importFrom stats sd
#'

lobanov_normalize = function (formants, speakers, vowels, warning = FALSE){
  if (is.null(ncol(formants)))
    stop("At least two formants must be provided (i.e. F1, F2, ...)")
  if (length(speakers) != nrow(formants))
    stop("Speaker vector length does not match formant data length.")
  if (length(vowels) != nrow(formants))
    stop("Formant vector length does not match formant data length.")
  if (warning)
    warning("Lobanov normalization is terrible, please don't use it other than to investigate how terrible it is. For more information on this, please see: \nBarreda, S. (2021). Perceptual validation of vowel normalization methods for variationist research. Language Variation and Change, 33(1), 27-53.", call. = FALSE)

  nffs = ncol(formants)
  speakers = as.factor(as.character(speakers))
  n_speakers = length(unique(speakers))
  vowels = as.factor(as.character(vowels))
  speakersf = levels(speakers)
  vowelsf = levels(vowels)

  for (j in 1:n_speakers) {
    temp = (speakers == speakersf[j])
    mff = NULL
    sdff = NULL
    for (i in 1:nffs) {
      mff = c(mff, mean(tapply(formants[temp, i], vowels[temp], mean)))
      sdff = c(sdff, sd(tapply(formants[temp, i], vowels[temp], mean)))
    }
    mffs = matrix(mff, nrow(formants[temp, ]), nffs, byrow = TRUE)
    sdffs = matrix(sdff, nrow(formants[temp, ]), nffs, byrow = TRUE)
    formants[temp, ] = (formants[temp, ] - mffs)/sdffs
  }
  output = data.frame(formants, speaker = speakers, vowel = vowels)
  return(output)
}
