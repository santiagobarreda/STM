#' Bayesian Sliding Template Model
#'
#' Estimate psi from individual tokens using an update to the algorithm
#' suggested by Nearey and Assmann (2007). Returns posterior probabilities for
#' each category based on the estimate of psi, the template, and token
#' acoustic properties.
#'
#' @param ffs A vector, matrix, or dataframe of log-transformed formant
#' frequencies. Each row is a different observation, each column is a formant
#' measurement.
#' @param f0 A vector of log-transformed fundamental frequencies, one per row
#' of \code{ffs}.
#' @param template A template object, created with \code{create_template()}
#' from normalized (log-mean-centered) formant data.
#' @param method A function that that estimates the 'best' psi given some information.
#' @param vowel_priors A vector of prior probabilities for each category. If
#' named, names must match \code{template$classes}; otherwise values must be in
#' the order of \code{template$classes}.
#' @param correctOUflow A boolean indicating whether to correct for underflow
#' and overflow using the correctOUflow() function.
#' @param lite A boolean indicating whether to return compact output for speed.
#' @param ... Additional arguments are passed to internal call of estimation method.
#' @return In the case of a single token (a vector, or a one-row matrix or
#' dataframe), an \code{STM_output} object. If several tokens are passed, an
#' \code{STM_output_list}. If \code{lite = TRUE}, a list with matrices
#' \code{psi} and \code{posterior}, one row per token.
#' @details With the built-in methods (\code{method6}, \code{method3},
#' \code{method2}), all tokens are processed together in one vectorized
#' pass, so passing many tokens at once is much faster than calling this
#' function once per token. User-supplied methods are called once per token.
#' When only psi estimates and posterior probabilities are needed (e.g. when
#' bootstrapping), \code{lite = TRUE} is faster still, since it skips
#' building an output object for each token.
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' nd = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' template = create_template(nd[, 1:3], nd$vowels)
#'
#' # formants and f0 must be log-transformed
#' ffs = log(d[1:5, c("f1", "f2", "f3")])
#' f0 = log(d$f0[1:5])
#'
#' out = BSTM(ffs[1, ], f0[1], template)
#' out
#'
#' res = BSTM(ffs, f0, template)
#' res$winners
#' res$posterior
#' }
#' @export

BSTM = function(ffs, f0, template, method = method6, vowel_priors=NULL,
                correctOUflow = TRUE, lite = FALSE, ...) {
  xstm_internal(ffs, f0, template, method = method, vowel_priors = vowel_priors,
                correctOUflow = correctOUflow, lite = lite, .type = "BSTM", ...)
}
