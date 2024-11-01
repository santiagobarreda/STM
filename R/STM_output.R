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
#' # TBD

STM_output <- function(df,stimulus, template, parameters,vowel_priors) {

  output = list()
  output$df = data.frame(df)
  output$stimulus = stimulus
  output$template = template
  output$parameters = parameters
  output$vowel_priors = vowel_priors
  output$parameters = parameters

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
}


#' @rdname STM_output
#' @param x An object of class "STM_output".
#' @param i Row index.
#' @param j Column index.
#' @return A subset of the data frame in the STM_output object.
#' @usage \method{[}{STM_output}(x, i, j)
#' @export
#' @method `[` STM_output
#' @aliases [.STM_output
#'

`[.STM_output` <- function(x, i, j) {
  return(x$df[i, j])
}




#' @rdname STM_output
#' @param x An object of class "STM_output".
#' @param y unused parameter
#' @param ... Additional arguments passed to other methods.
#' @export
#' @method plot STM_output
#'

plot.STM_output = function(x,y, ...) {

  plot_data = x$df

  plot_posterior(plot_data)

}
