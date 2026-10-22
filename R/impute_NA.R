#' impute_NA
#'
#' Imputes missing formant values with or without random error.
#'
#' Predicts log(formant)~speaker+vowel and imputes missing values.
#' If sim_error is TRUE, imputes with random error by simulating normal
#' data with a mean of zero and a standard deviation equal to the residual
#' error of the regression model.
#'
#' Formants may be given in Hz or already log-transformed (data whose maximum
#' is 20 or less are assumed to be logged). Values of \code{NA} or zero are
#' treated as missing.
#'
#' @param formants A matrix or data frame of formant frequencies
#' @param speakers A vector of speaker IDs
#' @param vowels A vector of vowel IDs
#' @param sim_error A logical indicating whether to simulate error.
#' @return An object of the same type and shape as \code{formants}, with
#' missing values replaced. Observed values are unchanged. Imputed values are
#' on the same scale as the input: rounded Hz for Hz input, log Hz for
#' logged input.
#' @examples
#' \dontrun{
#' formants = h95_data[, c("f1", "f2", "f3")]
#' sum(is.na(formants))
#' imputed = impute_NA(formants, h95_data$speaker, h95_data$vowel)
#' sum(is.na(imputed))
#' }
#' @export


impute_NA = function (formants, speakers, vowels, sim_error = FALSE){
    normalize_impute(formants, speakers, vowels, normalize = FALSE,
                     impute = TRUE, sim_error = sim_error)
}
