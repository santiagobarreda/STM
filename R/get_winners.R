
#' Get the winners of the BSTM model
#'
#' This function returns the winning psi and vowel for each token in the BSTM model.
#' It accepts a list of results and returns a data frame with columns for the psi
#' and vowel.
#'
#' @param results_list A list of results from a BSTM or PSTM model: an
#' \code{STM_output_list}, or a plain list of \code{STM_output} objects.
#' @return A data frame with columns for the psi and vowel.
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' nd = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' template = create_template(nd[, 1:3], nd$vowels)
#' res = BSTM(log(d[1:5, c("f1", "f2", "f3")]), log(d$f0[1:5]), template)
#' get_winners(res)
#' }
#' @export
#'


get_winners = function (results_list) {
  results_list = check_output_list(results_list)
  winner = vapply(results_list, function(x) which.max(x$df$posterior_probability),
                  integer(1))
  psi_hat = mapply(function(x, w) x$df$posterior_mu[w], results_list, winner)
  vowel_hat = mapply(function(x, w) rownames(x$df)[w], results_list, winner)
  output = data.frame(psi_hat = unname(psi_hat), vowel_hat = unname(vowel_hat),
                      stringsAsFactors = FALSE)
  return (output)
}


# Return a plain list of STM_output objects, or stop.
#' @noRd

check_output_list = function(results_list){
  results_list = unclass(results_list)
  attr(results_list, "winners") = NULL
  attr(results_list, "posterior") = NULL
  if (!is.list(results_list) ||
      !all(vapply(results_list, inherits, logical(1), what = "STM_output")))
    stop("results_list must be an STM_output_list or a list of STM_output objects.",
         call. = FALSE)
  return(results_list)
}
