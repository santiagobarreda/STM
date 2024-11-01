
#' Find the posterior distribution
#'
#' This function calculates the posterior distribution for a given set of
#' likelihoods and priors. It accepts a matrix of likelihoods and a matrix of
#' priors and returns a matrix of posteriors. The function also accepts a string
#' indicating the type of method to use for estimating the posterior. The
#' function returns a matrix of posteriors with columns for the mean, standard
#' deviation, density, probability, and rounded probability.
#'
#' @param likelihoods A matrix of likelihoods with columns for the mean, standard deviation, and density.
#' @param priors A matrix of priors with columns for the mean, standard deviation, and density.
#' @param type A string indicating the type of method to use for estimating the posterior.
#' @return A matrix of posteriors with columns for the mean, standard deviation, density, probability, and rounded probability.
#' @examples
#' # TBD
#' @export
#'

find_posterior = function(likelihoods, priors, type = "BSTM"){
  n_vs = nrow(likelihoods)

  posterior = matrix(0,n_vs,5,byrow=TRUE)
  posterior[,1:3] = combine_gaussians(likelihoods[,1],likelihoods[,2],
                                      priors[,1],priors[,2],
                                      likelihoods[,3],priors[,3])
  posterior = scale_posterior(posterior, type=type)
  colnames(posterior) = c('posterior_mu', 'posterior_sd', 'posterior_density',
                          'posterior_probability', 'rounded_pp')
  rownames (posterior) = rownames(likelihoods)
  return(posterior)
}
