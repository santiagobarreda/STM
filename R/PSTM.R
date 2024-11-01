#' Probabilistic Sliding Template Model
#'
#' Estimate psi from individual tokens using an update to the algorithm
#' suggested by Nearey and Assmann (2007). Returns posterior probabilities for
#' each category based on the estimate of psi, the template, and token
#' acoustic properties.
#'
#' @param ffs A vector, matrix, or dataframe of formant frequencies. Each row
#' is a different observation, each column is a formant measurement.
#' @param f0 A vector of fundamental frequencies
#' @param template A template object
#' @param method A function that that estimates the 'best' psi given some information.
#' @param vowel_priors A vector of prior probabilities for each category
#' @param correctOUflow A boolean indicating whether to correct for underflow
#' and overflow using the correctOUflow() function.
#' @param ... Additional arguments are passed to internal call of estimation method.
#' @return In the case of a single token, a dataframe of psi estimates and
#' posterior probabilites. If several tokens (i.e. rows) are passed, output is
#' a list of dataframes.
#' @examples
#' # TBD
#' @export

PSTM = function(ffs, f0, template, method = method6, vowel_priors=NULL,correctOUflow = TRUE, ...) {

  if (is.vector(ffs)) {
    # If ffs is a vector, call method directly
    if (length(f0)>1) cat("Warning: Using first element of f0 vector.\n")
    return(ffs=method(unlist(ffs), f0=f0[1], template=template,
                      method=method, vowel_priors=vowel_priors,
                      correctOUflow=correctOUflow, ...))

  } else if (is.matrix(ffs)) {
    # If ffs is a matrix, apply method to each row of the matrix

    if (length(f0)!=nrow(ffs)) stop("nrow(ffs) must equal length(f0).\n")

    results <- apply(ffs, 1, function(row)
      method(ffs=row, f0=f0, template=template, method=method,
             vowel_priors=vowel_priors,correctOUflow=correctOUflow,
             type ="PSTM", ...))
    results = STM_output_list(results)
    return(results)

  } else if (is.data.frame(ffs)) {
    # If ffs is a dataframe

    if (length(f0)!=nrow(ffs)) stop("nrow(ffs) must equal length(f0).\n")

    if (nrow(ffs)==1)
      return(ffs=method(unlist(ffs), f0=f0[1], template=template,
                        method=method, vowel_priors=vowel_priors,
                        correctOUflow=correctOUflow,type ="PSTM", ...))

    ffs_matrix <- as.matrix(ffs)
    results <- lapply(1:nrow(ffs_matrix), function(i)
      method(ffs=ffs_matrix[i,], f0=f0[i], template=template, method=method,
             vowel_priors=vowel_priors,correctOUflow=correctOUflow,type ="PSTM"))
    results = STM_output_list(results)
    return(results)

  } else {
    stop("Unsupported input type. Please provide a vector, matrix, or dataframe.")
  }
}

