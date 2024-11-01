
#' Get posterior probabilities from a list of BSTM results
#'
#' This function takes a list of BSTM results and returns a matrix of posterior
#' probabilities. The function returns a matrix with columns for each category
#' and rows for each token.
#'
#' @param results_list A list of BSTM results
#' @return A matrix of posterior probabilities with columns for each category
#' and rows for each token.
#' @examples
#' # TBD
#' @export
#'

get_posterior = function (results_list){
  if (inherits(results_list, "STM_output_list")){
    output = t(sapply (results_list$list, function(results_list)
      results_list$df$posterior_probability))
    colnames (output) = rownames(results_list$list[[1]]$df)
    output = STM_posteriors(output)
    return (output)
  }
}


