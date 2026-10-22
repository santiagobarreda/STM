
# Internal helpers shared by BSTM(), PSTM(), STM() and the estimation methods.


# Validate vowel priors against the template classes. Named priors are
# reordered to match template$classes; unnamed priors are assumed to already
# be in that order.
#' @noRd

check_vowel_priors = function(vowel_priors, classes){
  if (is.null(vowel_priors)) return(NULL)

  if (!is.numeric(vowel_priors) || length(vowel_priors) != length(classes))
    stop("vowel_priors must be a numeric vector with one value per template class (",
         length(classes), ").", call. = FALSE)
  if (any(!is.finite(vowel_priors)) || any(vowel_priors < 0) || sum(vowel_priors) <= 0)
    stop("vowel_priors must be finite, non-negative, and not all zero.", call. = FALSE)

  if (!is.null(names(vowel_priors))){
    if (!setequal(names(vowel_priors), classes))
      stop("names(vowel_priors) must match template$classes.", call. = FALSE)
    vowel_priors = vowel_priors[classes]
  }
  return(vowel_priors)
}


# Likelihood of psi for each template class, one row per class.
#' @noRd

template_likelihoods = function(ffs, template){
  ffs = as.numeric(unlist(ffs))
  n_classes = nrow(template$means)
  likelihoods = t(vapply(seq_len(n_classes), function(j){
    estimate_likelihood(ffs, template$means[j,], template$covariance[[j]],
                        template$precision[[j]], template$log_det[[j]])
  }, numeric(3)))
  dimnames(likelihoods) = list(rownames(template$means),
                               c("likelihood_mu", "likelihood_sd", "likelihood_density"))
  return(likelihoods)
}


# Apply the underflow/overflow correction and keep rounded_pp in sync.
#' @noRd

finalize_posterior = function(posterior, correctOUflow){
  if (correctOUflow)
    posterior[, "posterior_probability"] =
      correctOUflow_internal(posterior[, "posterior_probability"])
  posterior[, "rounded_pp"] = round(posterior[, "posterior_probability"], 4)
  return(posterior)
}


# Shared driver for BSTM() and PSTM().
#' @noRd

xstm_internal = function(ffs, f0, template, method, vowel_priors, correctOUflow,
                         lite, .type, ...){

  dots = list(...)
  if (!is.null(dots$type)){
    warning("'type' is set by BSTM()/PSTM() and was ignored.", call. = FALSE)
    dots$type = NULL
  }
  vowel_priors = check_vowel_priors(vowel_priors, template$classes)

  call_method = function(ffs_i, f0_i){
    do.call(method, c(list(ffs = ffs_i, f0 = f0_i, template = template,
                           vowel_priors = vowel_priors,
                           correctOUflow = correctOUflow, type = .type,
                           lite = lite), dots))
  }

  if (is.data.frame(ffs)){
    ffs = as.matrix(ffs)
  } else if (is.vector(ffs)){
    if (length(f0) > 1) warning("Using first element of f0 vector.", call. = FALSE)
    f0 = f0[1]
    ffs = unlist(ffs)
    ffs = matrix(ffs, nrow = 1, dimnames = list(NULL, names(ffs)))
  } else if (!is.matrix(ffs)){
    stop("Unsupported input type. Please provide a vector, matrix, or dataframe.",
         call. = FALSE)
  }
  if (!is.numeric(ffs)) stop("ffs must be numeric.", call. = FALSE)
  if (length(f0) != nrow(ffs)) stop("nrow(ffs) must equal length(f0).", call. = FALSE)

  results = lapply(seq_len(nrow(ffs)), function(i) call_method(ffs[i, ], f0[i]))

  if (lite){
    psi = do.call(rbind, lapply(results, function(x) x$psi))
    posterior = do.call(rbind, lapply(results, function(x) x$posterior))
    rownames(psi) = seq_len(nrow(psi))
    rownames(posterior) = seq_len(nrow(posterior))
    return(list(psi = psi, posterior = posterior))
  }

  if (length(results) == 1) return(results[[1]])
  return(STM_output_list(results))
}
