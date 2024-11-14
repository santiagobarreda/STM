#' Create an object of class "STM_posteriors"
#'
#' This function creates an object of the S3 class "STM_posteriors", the output of the XSTM functions.
#'
#' @param mat the matrix of posteriors
#' @return An object of class "STM_posteriors".
#' @export
#'
#' @examples
#' # TBD

STM_posteriors <- function(mat) {

  output = list()
  output$df = mat
  class(output) = "STM_posteriors"
  return(output)
}


#' @rdname STM_posteriors
#' @param x An object of class "STM_posteriors".
#' @param digits Number of digits to print.
#' @param ... Additional arguments passed to other methods.
#' @export
#' @method print STM_posteriors
#'

print.STM_posteriors = function(x, digits = 3, ...) {

  print_data = x$df

  # Loop through columns and round format numeric columns
  for (i in 1:ncol(print_data)){
    print_data[,i] = round(print_data[,i], digits)
  }
  print(as.data.frame(print_data))
}


#' @rdname STM_posteriors
#' @param x An object of class "STM_posteriors".
#' @param i Row index.
#' @param j Column index.
#' @return A subset of the data frame in the STM_posteriors object.
#' @usage \method{[}{STM_posteriors}(x, i, j)
#' @export
#' @method `[` STM_posteriors
#' @aliases [.STM_posteriors
#'

`[.STM_posteriors` <- function(x, i, j) {
  return(x$df[i, j])
}



#' @rdname STM_posteriors
#' @param X An object of class "STM_posteriors".
#' @param MARGIN A vector giving the subscripts which the function will be applied over.
#' @param FUN The function to be applied.
#' @param ... Additional arguments passed to other methods.
#' @export
#' @method apply STM_posteriors
#'

apply.STM_posteriors = function(X, MARGIN, FUN, ...) {
  apply(X$df, MARGIN, FUN, ...)
}



