
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


# Warn when formant columns have the template's names but in a different
# order. The "formants." prefix added by normalize() is ignored.
#' @noRd

check_feature_names = function(ffs_names, template){
  template_names = template$features
  if (is.null(ffs_names) || is.null(template_names)) return(invisible(NULL))
  a = sub("^formants[.]", "", ffs_names)
  b = sub("^formants[.]", "", template_names)
  if (length(a) == length(b) && setequal(a, b) && !identical(a, b))
    warning("The formant columns are in a different order (", paste(ffs_names, collapse = ", "),
            ") from the template's (", paste(template_names, collapse = ", "),
            "). Columns are matched by position, not by name.", call. = FALSE)
  invisible(NULL)
}


# Warn when inputs look like Hz but the model expects log values: formants
# above 20 with a template on a log scale, or (for method6) f0 above 20 with
# the default f0 parameters, which assume log f0.
#' @noRd

check_log_scale = function(ffs, f0, template, prior, params){
  if (any(ffs > 20, na.rm = TRUE) && all(abs(template$means) <= 20))
    warning("Some formant values are above 20, so they look like Hz, but the template ",
            "is on a log scale. Formants should be log-transformed.", call. = FALSE)
  if (prior == "f0" && any(f0 > 20, na.rm = TRUE)){
    f0_params = c("f0_hat_sd", "f0_hat_intercept", "f0_hat_slope")
    defaults = unlist(lapply(formals(method6)[f0_params], eval))
    if (isTRUE(all.equal(unname(params[f0_params]), unname(defaults))))
      warning("Some f0 values are above 20, so they look like Hz. The default f0 ",
              "parameters of method6 assume log-transformed f0.", call. = FALSE)
  }
  invisible(NULL)
}


# Warn about arguments that a method did not recognize (they would otherwise
# be silently ignored, e.g. a misspelled prior parameter).
#' @noRd

warn_unused_args = function(dots, method_name){
  if (length(dots) == 0) return(invisible(NULL))
  arg_names = names(dots)
  if (is.null(arg_names)) arg_names = rep("", length(dots))
  arg_names[arg_names == ""] = "<unnamed>"
  warning("Unused argument(s) ignored by ", method_name, ": ",
          paste(arg_names, collapse = ", "), call. = FALSE)
  invisible(NULL)
}


# Shared driver for BSTM() and PSTM().
#' @noRd

xstm_internal = function(ffs, f0, template, method, vowel_priors, correctOUflow,
                         lite, .type, ...){

  dots = list(...)
  if (!is.null(dots[["type"]])){
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
  check_feature_names(colnames(ffs), template)

  # Built-in methods: process all tokens at once
  spec = batch_spec(method)
  if (!is.null(spec)){
    matched = batch_params(method, spec,
                           c(list(ffs = NULL, f0 = NULL, template = NULL,
                                  vowel_priors = vowel_priors,
                                  correctOUflow = correctOUflow, type = .type,
                                  lite = lite), dots))
    warn_unused_args(matched$unused, spec$name)
    params = matched$params
    check_log_scale(ffs, f0, template, spec$prior, params)
    batch = xstm_batch(ffs, f0, template, spec$prior, params, vowel_priors, .type,
                       correctOUflow)
    parameters = if (length(params) == 0) NA else params
    results = batch_results(batch, ffs, f0, template, parameters, vowel_priors, lite)
    if (lite) return(results)
    if (length(results$outputs) == 1) return(results$outputs[[1]])
    return(new_STM_output_list(results$outputs, results$winners, results$posterior))
  }

  # User-supplied methods: one token at a time
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
