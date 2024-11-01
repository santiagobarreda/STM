#' model_likelihood
#'
#' Calculate the likelihood of a model given classifications and posterior probabilities
#'
#' @param classifications A vector of classifications
#' @param posterior_probabilities A vector of posterior probabilities.
#' @param correctOUflow A boolean indicating whether to correct for underflow.
#' @return A scalar, the likelihood of the model given the classifications and posterior probabilities.
#' @examples
#' # Example usage:
#' # TBD
#' @export


model_likelihood = function (classifications, posterior_probabilities, correctOUflow = TRUE){

  if(correctOUflow) posterior_probabilities = correctOUflow_internal(posterior_probabilities)

  output = sum (classifications*log(posterior_probabilities))

  return (output)
}


