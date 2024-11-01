
#' @title correctOUflow_internal
#'
#' @description Correct for underflow and overflow in posterior probabilities
#'
#' @param posterior_probabilities A vector of posterior probabilities
#' @return A vector of posterior probabilities
#' @export
# @examples
# # TBD


correctOUflow_internal = function (posterior_probabilities){

  if (!is.null(nrow(posterior_probabilities)) && nrow(posterior_probabilities) > 0){
    if (nrow(posterior_probabilities) > 1)
      posterior_probabilities = t(apply(posterior_probabilities,1,correctOUflow_internal_))
    if (nrow(posterior_probabilities) == 1)
      posterior_probabilities = correctOUflow_internal_(posterior_probabilities)
  } else {
    posterior_probabilities = correctOUflow_internal_(posterior_probabilities)
  }

  return (posterior_probabilities)
}

#' @keywords internal

correctOUflow_internal_ = function (posterior_probabilities){
  posterior_probabilities[posterior_probabilities == 0] = 1 - 0.9999999999999998889777
  posterior_probabilities = posterior_probabilities / sum(posterior_probabilities)
  return(posterior_probabilities)
}


