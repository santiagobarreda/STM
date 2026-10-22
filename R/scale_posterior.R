#' Scale posterior probabilities to sum to 1
#'
#' This function scales the posterior probabilities to sum to 1.
#' It accepts a matrix of posterior probabilities and add columns to the matrix
#' representing posteriors, and modifying posterior densities as required.
#'
#' @param posterior A matrix of posterior probabilities
#' with columns for each category and rows for each token.
#' @param type A string indicating how the posterior should be calculated,
#' either "BSTM" or "PSTM".
#' @return A matrix of posterior probabilities with an additional column
#' representing the scaled posterior probabilities.
#' @examples
#' # TBD
#' @export
#'

scale_posterior = function(posterior, type = "BSTM"){
  if (type == "BSTM"){
    # peak density * sd * sqrt(2*pi) is integral of each posterior
    vowel_integrals = posterior[,3] + log(posterior[,2]) + (log(pi) + log(2)) * (0.5)
    # log of total integral for each psi for each vowel
    max_integral = max(vowel_integrals)
    log_sum_vowel_integrals = max_integral +
      log(sum(exp(vowel_integrals - max_integral)))
    # subtract log (i.e. divide) total integral from each vowel integral
    posterior[,3] = posterior[,3] - log_sum_vowel_integrals
    posterior[,4] = exp(vowel_integrals - log_sum_vowel_integrals)
  }

  if (type == "PSTM"){
    max_density = max(posterior[,3])
    scaled_density = exp(posterior[,3] - max_density)
    posterior[,4] = scaled_density / sum(scaled_density)
  }
  posterior[,5] = round (posterior[,4], 4)
  return(posterior)
}


