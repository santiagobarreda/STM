#' Create an object of class "STM_output"
#'
#' This function creates an object of the S3 class "STM_output", the output of the XSTM functions.
#'
#' @param df the dataframe
#' @param stimulus the stimulus
#' @param template the template
#' @param parameters the parameters
#' @param vowel_priors the vowel priors
#' @return An object of class "STM_output".
#' @export
#'
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' nd = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' template = create_template(nd[, 1:3], nd$vowels)
#' out = BSTM(log(d[1, c("f1", "f2", "f3")]), log(d$f0[1]), template)
#' class(out)
#' out[, c("posterior_mu", "posterior_probability")]
#' plot(out)
#' }

STM_output <- function(df,stimulus, template, parameters,vowel_priors) {

  output = list()
  output$df = if (is.data.frame(df)) df else data.frame(df)
  output$stimulus = stimulus
  output$template = template
  output$parameters = parameters
  output$vowel_priors = vowel_priors

  class(output) = "STM_output"
  return(output)
}


#' @rdname STM_output
#' @param x An object of class "STM_output".
#' @param digits Number of digits to print.
#' @param ... Additional arguments passed to other methods.
#' @export
#' @method print STM_output
#'

print.STM_output = function(x, digits = 3, ...) {

  print_data = x$df

  # Loop through columns and round format numeric columns
  for (i in 1:ncol(print_data)) {
   if (is.numeric(print_data[,i])) {
     print_data[,i] = format(round(print_data[,i], digits), nsmall = digits)
   }
  }

  print(as.data.frame(print_data))
  invisible(x)
}


#' @rdname STM_output
#' @param x An object of class "STM_output".
#' @param i Row index (or column index, if \code{j} is not given).
#' @param j Column index.
#' @param drop Passed to the data frame subsetting method.
#' @return A subset of the data frame in the STM_output object, following data
#' frame semantics: \code{x[i]} selects columns, \code{x[i, j]} selects rows
#' and columns.
#' @usage \method{[}{STM_output}(x, i, j, drop)
#' @export
#' @method `[` STM_output
#' @aliases [.STM_output
#'

`[.STM_output` <- function(x, i, j, drop) {
  n_args = nargs() - !missing(drop)
  if (n_args == 2) return(x$df[i])
  if (missing(drop)) return(x$df[i, j])
  return(x$df[i, j, drop = drop])
}




#' @rdname STM_output
#' @param x An object of class "STM_output".
#' @param y unused parameter
#' @param ... Additional arguments passed to \code{plot_posterior()}, e.g.
#' \code{psi_range}.
#' @export
#' @method plot STM_output
#'

plot.STM_output = function(x,y, ...) {

  plot_data = x$df

  plot_posterior(plot_data, ...)

}
