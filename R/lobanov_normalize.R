
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
#' \dontrun{
#' d = h95_data_no_NA
#' out = lobanov_normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' head(out)
#' }
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

  formants = as.matrix(formants)
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
      # vowel means for this speaker; vowels the speaker did not produce are NA
      vowel_means = tapply(formants[temp, i], vowels[temp], mean, na.rm = TRUE)
      mff = c(mff, mean(vowel_means, na.rm = TRUE))
      sdff = c(sdff, sd(vowel_means, na.rm = TRUE))
    }
    mffs = matrix(mff, sum(temp), nffs, byrow = TRUE)
    sdffs = matrix(sdff, sum(temp), nffs, byrow = TRUE)
    formants[temp, ] = (formants[temp, , drop = FALSE] - mffs)/sdffs
  }
  output = data.frame(formants, speaker = speakers, vowel = vowels)
  return(output)
}
