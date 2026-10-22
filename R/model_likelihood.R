#' model_likelihood
#'
#' Calculate the likelihood of a model given classifications and posterior probabilities
#'
#' @param classifications A matrix or data frame of classification counts
#' (one row per token, one column per category), or a vector of counts.
#' @param posterior_probabilities Posterior probabilities with the same shape
#' as \code{classifications}: a matrix, data frame, or \code{STM_posteriors}
#' object (one row per token), or a vector. If both inputs have column names,
#' the posterior columns are matched to the classification columns by name.
#' @param correctOUflow A boolean indicating whether to correct for underflow.
#' For matrix input, zeros are replaced and each row is rescaled to sum to one.
#' For vector input, zeros are replaced but no rescaling is done, since the
#' elements need not belong to the same token.
#' @return A scalar, the log likelihood of the model given the classifications
#' and posterior probabilities.
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' nd = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' template = create_template(nd[, 1:3], nd$vowels)
#' post = STM(nd[, 1:3], template)
#' model_likelihood(h95_classifications, post)
#' }
#' @export


model_likelihood = function (classifications, posterior_probabilities, correctOUflow = TRUE){

  if (inherits(posterior_probabilities, "STM_posteriors"))
    posterior_probabilities = posterior_probabilities$df
  if (is.data.frame(posterior_probabilities))
    posterior_probabilities = as.matrix(posterior_probabilities)
  if (is.data.frame(classifications))
    classifications = as.matrix(classifications)

  if (is.null(dim(posterior_probabilities))){
    if (length(classifications) != length(posterior_probabilities))
      stop("classifications and posterior_probabilities must have the same length.",
           call. = FALSE)
    if (correctOUflow)
      posterior_probabilities[posterior_probabilities == 0] = .Machine$double.eps
  } else {
    if (!identical(dim(classifications), dim(posterior_probabilities)))
      stop("classifications and posterior_probabilities must have the same dimensions.",
           call. = FALSE)
    cl_names = colnames(classifications)
    pp_names = colnames(posterior_probabilities)
    if (!is.null(cl_names) && !is.null(pp_names) && !identical(cl_names, pp_names)){
      if (!setequal(cl_names, pp_names))
        stop("Column names of classifications and posterior_probabilities do not match.",
             call. = FALSE)
      posterior_probabilities = posterior_probabilities[, cl_names]
    }
    if (correctOUflow)
      posterior_probabilities = correctOUflow_internal(posterior_probabilities)
  }

  # cells with no responses contribute nothing, even if their probability is 0
  responded = classifications != 0
  output = sum (classifications[responded]*log(posterior_probabilities[responded]))

  return (output)
}
