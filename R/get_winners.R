
#' Get the winners of the BSTM model
#'
#' This function returns the winning psi and vowel for each token in the BSTM model.
#' It accepts a list of results and returns a data frame with columns for the psi
#' and vowel.
#'
#' @param results_list A list of results from a BSTM or PSTM model.
#' @return A data frame with columns for the psi and vowel.
#' @examples
#' # TBD
#' @export
#'


get_winners = function (results_list) {
  if (inherits(results_list, "STM_output_list")){
    output = data.frame(t(sapply (results_list$list, function(x){
      winner = which.max(x$df$posterior_probability)
      psi_hat = x$df$posterior_mu[winner]
      vowel_hat = rownames(x$df)[winner]
      c(psi_hat=as.numeric(psi_hat), vowel_hat=vowel_hat)
      })))
    output[,1] = as.numeric (output[,1])
    return (output)
  }
}
