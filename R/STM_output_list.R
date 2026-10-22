#' Create an object of class "STM_output_list"
#'
#' This function creates an object of the S3 class "STM_output_list", the a list of outputs
#' of the XSTM functions.
#'
#' @param output_list A list of STM_output objects.
#' @return An object of class "STM_output_list".
#' @export
#'
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' nd = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' template = create_template(nd[, 1:3], nd$vowels)
#' res = BSTM(log(d[1:5, c("f1", "f2", "f3")]), log(d$f0[1:5]), template)
#' length(res)
#' res$winners
#' res[[1]]
#' res[2:3]
#' }

STM_output_list <- function(output_list) {
  class(output_list) = "STM_output_list"
  attr(output_list, "winners") = get_winners(output_list)
  attr(output_list, "posterior") = get_posterior(output_list)
  return(output_list)
}


#' @rdname STM_output_list
#' @param x An object of class "STM_output_list".
#' @param ... Additional arguments passed to other methods.
#' @export
#' @method print STM_output_list
#'

print.STM_output_list = function(x, ...) {

  cat ("\nSTM_output_list object with the following elements:\n\n")
  cat ("$list: a list of",length(x),"STM_output objects\n")
  cat ("$winners: a data frame of the winning psi and vowel for each token\n")
  cat ("$posterior: a data frame of the posterior probabilities for each token\n\n")
}


#' @rdname STM_output_list
#' @param x An object of class "STM_output_list".
#' @param name Element name.
#' @export
#' @method $ STM_output_list
#'

`$.STM_output_list` <- function(x, name) {
  if (name == "list") {
    output = unclass(x)
    attr(output, "winners") = NULL
    attr(output, "posterior") = NULL
    return(output)
  }

  if (name == "winners") return(attr(x, "winners"))
  if (name == "posterior") return(attr(x, "posterior"))

  NextMethod()
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
  return(unclass(x)[[i]])
}


#' @rdname STM_output_list
#' @param x An object of class "STM_output_list".
#' @export
#' @method length STM_output_list
#'

length.STM_output_list = function(x) {
  length(unclass(x))
}


#' @rdname STM_output_list
#' @param x An object of class "STM_output_list".
#' @param i index.
#' @return A new STM_output_list containing the selected elements.
#' @usage \method{[}{STM_output_list}(x, i)
#' @export
#' @method `[` STM_output_list
#' @aliases [.STM_output_list
#'

`[.STM_output_list` <- function(x, i) {
  return(STM_output_list(unclass(x)[i]))
}



