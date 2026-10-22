#' Create an object of class "STM_posteriors"
#'
#' This function creates an object of the S3 class "STM_posteriors", the output of the XSTM functions.
#'
#' @param mat the matrix of posteriors
#' @return An object of class "STM_posteriors".
#' @export
#'
#' @examples
#' \dontrun{
#' mat = matrix(runif(12), nrow = 3)
#' mat = mat / rowSums(mat)
#' post = STM_posteriors(mat)
#' post
#' post[1, ]
#' nrow(post)
#' }

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
#' @param drop Passed to the matrix subsetting method.
#' @return A subset of the posterior matrix in the STM_posteriors object,
#' following matrix semantics.
#' @usage \method{[}{STM_posteriors}(x, i, j, drop)
#' @export
#' @method `[` STM_posteriors
#' @aliases [.STM_posteriors
#'

`[.STM_posteriors` <- function(x, i, j, drop = TRUE) {
  n_args = nargs() - !missing(drop)
  if (n_args == 2) return(x$df[i])
  return(x$df[i, j, drop = drop])
}


#' @rdname STM_posteriors
#' @param x An object of class "STM_posteriors".
#' @param ... Additional arguments passed to other methods.
#' @details \code{dim()}, \code{dimnames()} and \code{as.matrix()} methods
#' are provided, so \code{nrow()}, \code{ncol()} and \code{apply()} work
#' directly on STM_posteriors objects.
#' @export
#' @method as.matrix STM_posteriors
#'

as.matrix.STM_posteriors = function(x, ...) {
  x$df
}


#' @rdname STM_posteriors
#' @export
#' @method dim STM_posteriors
#'

dim.STM_posteriors = function(x) {
  dim(x$df)
}


#' @rdname STM_posteriors
#' @export
#' @method dimnames STM_posteriors
#'

dimnames.STM_posteriors = function(x) {
  dimnames(x$df)
}



