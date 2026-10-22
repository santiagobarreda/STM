
#' @title correctOUflow_internal
#'
#' @description Correct for underflow and overflow in posterior probabilities.
#' Probabilities of exactly zero are replaced by \code{.Machine$double.eps} and
#' probabilities are rescaled to sum to one: across the whole vector for a
#' vector input, or within each row for a matrix input.
#'
#' @param posterior_probabilities A vector of posterior probabilities for one
#' token, or a matrix (or \code{STM_posteriors} object) with one row per token.
#' @return Posterior probabilities with the same shape as the input.
#' @examples
#' \dontrun{
#' correctOUflow_internal(c(0, 0.4, 0.6))
#' correctOUflow_internal(rbind(c(0, 0.4, 0.6), c(0.2, 0.3, 0.5)))
#' }
#' @export


correctOUflow_internal = function (posterior_probabilities){
  if (inherits(posterior_probabilities, "STM_posteriors"))
    posterior_probabilities = posterior_probabilities$df
  if (is.data.frame(posterior_probabilities))
    posterior_probabilities = as.matrix(posterior_probabilities)

  posterior_probabilities[posterior_probabilities == 0] = .Machine$double.eps

  if (is.null(dim(posterior_probabilities)))
    return (posterior_probabilities / sum(posterior_probabilities))
  return (posterior_probabilities / rowSums(posterior_probabilities))
}
