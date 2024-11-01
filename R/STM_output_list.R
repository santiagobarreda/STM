#' Create an object of class "STM_output_list"
#'
#' This function creates an object of the S3 class "STM_output_list", the a list of outputs
#' of the XSTM functions.
#'
#' @param output_list A list of STM_output objects.
#' @return An object of class "STM_output".
#' @export
#'
#' @examples
#' # TBD

STM_output_list <- function(output_list) {

  output = list()
  output$list = output_list
  output$winners = get_winners(output_list)
  output$posterior = get_posterior(output_list)

  class(output) = "STM_output_list"
  return(output)
}


#' @rdname STM_output_list
#' @param x An object of class "STM_output_list".
#' @param ... Additional arguments passed to other methods.
#' @export
#' @method print STM_output_list
#' @examples
#' # TBD
#'

print.STM_output_list = function(x, ...) {

  cat ("\nSTM_output_list object with the following elements:\n\n")
  cat ("$list: a list of",length(x$list),"STM_output objects\n")
  cat ("$winners: a data frame of the winning psi and vowel for each token\n")
  cat ("$posterior: a data frame of the posterior probabilities for each token\n\n")
}


#' @rdname STM_output_list
#' @param x An object of class "STM_output_list".
#' @param i index.
#' @return A subset of the data frame in the STM_output_list object.
#' @usage \method{[[}{STM_output_list}(x, i)
#' @export
#' @method `[[` STM_output_list
#' @aliases [[.STM_output_list
#'

`[[.STM_output_list` <- function(x, i) {
  return(x$list[[i]])
}


#' @rdname STM_output_list
#' @param X An object of class "STM_output_list".
#' @param FUN The function to be applied.
#' @param ... Additional arguments passed to other methods.
#' @param simplify A logical indicating whether to simplify the result.
#' @param USE.NAMES A logical indicating whether to use names.
#' @export
#' @method sapply STM_output_list
#'

sapply.STM_output_list = function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE){
  sapply(X[[1]], FUN, ...)
}



