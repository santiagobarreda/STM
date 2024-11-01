#' impute_NA
#'
#' Imputes missing formant values with or without random error.
#'
#' Predicts log(formant)~speaker+vowel and imputes missing values.
#' If sim_error is TRUE, imputes with random error by simulating normal
#' data with a mean of zero and a standard deviation equal to the residual
#' error of the regression model.
#'
#' @param formants A matrix of formant frequencies
#' @param speakers A vector of speaker IDs
#' @param vowels A vector of vowel IDs
#' @param sim_error A logical indicating whether to simulate error.
#' @return What your function returns.
#' @examples
#' # Example usage:
#' # TBD
#' @export


impute_NA = function (formants, speakers, vowels, sim_error = FALSE){
    normalize_impute(formants, speakers, vowels, normalize = FALSE,
                     impute = TRUE, sim_error = sim_error)
}


