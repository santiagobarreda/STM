#' Bayesian Sliding Template Model
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
#' @param lite A boolean indicating whether to return compact output for speed.
#' @param ... Additional arguments are passed to internal call of estimation method.
#' @return In the case of a single token, a dataframe of psi estimates and
#' posterior probabilites. If several tokens (i.e. rows) are passed, output is
#' a list of dataframes.
#' @examples
#' # TBD
#' @export


BSTM = function(ffs, f0, template, method = method6, vowel_priors=NULL,
                correctOUflow = TRUE, lite = FALSE, ...) {

  call_method = function(ffs_i, f0_i) {
    if (lite) {
      return(method(ffs=ffs_i, f0=f0_i, template=template,
                    vowel_priors=vowel_priors,
                    correctOUflow=correctOUflow, lite=TRUE, ...))
    }
    method(ffs=ffs_i, f0=f0_i, template=template,
           vowel_priors=vowel_priors,
           correctOUflow=correctOUflow, ...)
  }

  collect_lite_results = function(results) {
    psi = do.call(rbind, lapply(results, function(x) x$psi))
    posterior = do.call(rbind, lapply(results, function(x) x$posterior))
    rownames(psi) = seq_len(nrow(psi))
    rownames(posterior) = seq_len(nrow(posterior))
    list(psi = psi, posterior = posterior)
  }

  if (is.vector(ffs)) {
    # If ffs is a vector, call method directly
    if (length(f0)>1) cat("Warning: Using first element of f0 vector.\n")
    result = call_method(unlist(ffs), f0[1])
    if (lite) return(collect_lite_results(list(result)))
    return(result)

  } else if (is.matrix(ffs)) {
    # If ffs is a matrix, apply method to each row of the matrix

    if (length(f0)!=nrow(ffs)) stop("nrow(ffs) must equal length(f0).\n")

    results <- lapply(seq_len(nrow(ffs)), function(i)
      call_method(ffs[i, ], f0[i]))
    if (lite) return(collect_lite_results(results))
    results = STM_output_list(results)
    return(results)

  } else if (is.data.frame(ffs)) {
    # If ffs is a dataframe

    if (length(f0)!=nrow(ffs)) stop("nrow(ffs) must equal length(f0).\n")

    if (nrow(ffs)==1)
      return({
        result = call_method(unlist(ffs), f0[1])
        if (lite) collect_lite_results(list(result)) else result
      })

    ffs_matrix <- as.matrix(ffs)
    results <- lapply(seq_len(nrow(ffs_matrix)), function(i)
      call_method(ffs_matrix[i,], f0[i]))
    if (lite) return(collect_lite_results(results))
    results = STM_output_list(results)
    return(results)

  } else {
    stop("Unsupported input type. Please provide a vector, matrix, or dataframe.")
  }
}

