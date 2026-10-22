
#' Get posterior probabilities from a list of BSTM results
#'
#' This function takes a list of BSTM results and returns a matrix of posterior
#' probabilities. The function returns a matrix with columns for each category
#' and rows for each token.
#'
#' @param results_list A list of BSTM results: an \code{STM_output_list}, or a
#' plain list of \code{STM_output} objects.
#' @return An \code{STM_posteriors} object containing a matrix of posterior
#' probabilities with columns for each category and rows for each token.
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' nd = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' template = create_template(nd[, 1:3], nd$vowels)
#' res = BSTM(log(d[1:5, c("f1", "f2", "f3")]), log(d$f0[1:5]), template)
#' get_posterior(res)
#' }
#' @export
#'

get_posterior = function (results_list){
  results_list = check_output_list(results_list)
  output = do.call(rbind, lapply(results_list, function(result) {
    posterior = result$df$posterior_probability
    names(posterior) = rownames(result$df)
    posterior
  }))
  output = STM_posteriors(output)
  return (output)
}
