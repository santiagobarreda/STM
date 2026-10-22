
# Vectorized engine for the built-in estimation methods. All tokens are
# processed at once, with a handful of matrix operations per template class,
# instead of one R function call per token and class.
#
# prior: "f0" (method6, psi prior adjusted by f0), "fixed" (method3, a single
# gaussian prior on psi), or "none" (method2, uniform prior on psi).
#
# Returns a list of n x k matrices (tokens x classes), one per output column.
#' @noRd

xstm_batch = function(ffs, f0, template, prior, params, vowel_priors, type,
                      correctOUflow){
  n = nrow(ffs)
  d = ncol(ffs)
  k = nrow(template$means)
  classes = rownames(template$means)
  if (d != ncol(template$means))
    stop("ffs must have one column per template formant (", ncol(template$means), ").",
         call. = FALSE)
  log_2pi = log(2 * pi)

  # likelihood of psi for each token and class
  likelihood_mu = likelihood_sd = likelihood_density = matrix(0, n, k)
  for (j in seq_len(k)){
    precision = template$precision[[j]]
    log_det = template$log_det[[j]]
    if (is.null(precision) || is.null(log_det)){
      chol_factor = tryCatch(chol(template$covariance[[j]]), error = function(e)
        stop("Covariance matrix must be positive definite.", call. = FALSE))
      precision = chol2inv(chol_factor)
      log_det = 2 * sum(log(diag(chol_factor)))
    }
    total_precision = sum(precision)
    centered = ffs - matrix(template$means[j, ], n, d, byrow = TRUE)
    mu = drop(centered %*% rowSums(precision)) / total_precision
    centered = centered - mu
    quad_form = rowSums((centered %*% precision) * centered)
    likelihood_mu[, j] = mu
    likelihood_sd[, j] = sqrt(1 / total_precision)
    likelihood_density[, j] = -0.5 * (d * log_2pi + log_det + quad_form)
  }

  # prior on psi for each token and class
  if (prior == "f0"){
    p = f0_psi_prior(f0, params[["PSI_prior_mean"]], params[["PSI_prior_sd"]],
                     params[["f0_hat_sd"]], params[["f0_hat_intercept"]],
                     params[["f0_hat_slope"]])
    prior_mu = matrix(p[, "prior_mu"], n, k)
    prior_sd = matrix(p[, "prior_sd"], n, k)
    prior_density = matrix(p[, "prior_density"], n, k)
  } else if (prior == "fixed"){
    prior_mu = matrix(params[["PSI_prior_mean"]], n, k)
    prior_sd = matrix(params[["PSI_prior_sd"]], n, k)
    prior_density = matrix(log(1 / sqrt(params[["PSI_prior_sd"]]^2 * pi * 2)), n, k)
  } else {
    prior_mu = prior_sd = prior_density = matrix(NA_real_, n, k)
  }

  log_vowel_priors = if (is.null(vowel_priors)) rep(0, k) else log(vowel_priors)

  # posterior: product of the likelihood and prior gaussians
  if (prior == "none"){
    if (!is.null(vowel_priors))
      prior_density = matrix(log_vowel_priors, n, k, byrow = TRUE)
    posterior_mu = likelihood_mu
    posterior_sd = likelihood_sd
    posterior_density = likelihood_density +
      matrix(log_vowel_priors, n, k, byrow = TRUE)
  } else {
    prior_density = prior_density + matrix(log_vowel_priors, n, k, byrow = TRUE)
    combined_precision = 1 / likelihood_sd^2 + 1 / prior_sd^2
    posterior_mu = (likelihood_mu / likelihood_sd^2 + prior_mu / prior_sd^2) /
      combined_precision
    posterior_sd = sqrt(1 / combined_precision)
    posterior_density =
      dlike(posterior_mu, likelihood_mu, likelihood_sd, likelihood_density, log = TRUE) +
      dlike(posterior_mu, prior_mu, prior_sd, prior_density, log = TRUE)
  }

  # posterior probabilities, scaled within each token (row)
  if (type == "BSTM"){
    # peak density * sd * sqrt(2*pi) is the integral of each posterior
    vowel_integrals = posterior_density + log(posterior_sd) + 0.5 * log_2pi
    log_total = row_log_sum_exp(vowel_integrals)
    posterior_density = posterior_density - log_total
    posterior_probability = exp(vowel_integrals - log_total)
  } else {
    posterior_probability = exp(posterior_density - row_log_sum_exp(posterior_density))
  }
  if (correctOUflow){
    posterior_probability[posterior_probability == 0] = .Machine$double.eps
    posterior_probability = posterior_probability / rowSums(posterior_probability)
  }

  out = list(posterior_mu = posterior_mu, posterior_sd = posterior_sd,
             posterior_density = posterior_density,
             posterior_probability = posterior_probability,
             rounded_pp = round(posterior_probability, 4),
             likelihood_mu = likelihood_mu, likelihood_sd = likelihood_sd,
             likelihood_density = likelihood_density,
             prior_mu = prior_mu, prior_sd = prior_sd, prior_density = prior_density)
  lapply(out, function(m){ colnames(m) = classes; m })
}


# log(rowSums(exp(x))), computed stably.
#' @noRd

row_log_sum_exp = function(x){
  row_max = x[cbind(seq_len(nrow(x)), max.col(x, ties.method = "first"))]
  row_max + log(rowSums(exp(x - row_max)))
}


# Vectorized version of estimate_f0_psi_prior(); returns one row per f0.
#' @noRd

f0_psi_prior = function(f0, PSI_prior_mean, PSI_prior_sd, f0_hat_sd,
                        f0_hat_intercept, f0_hat_slope){
  intercept_f0_given_psi = ((f0_hat_slope*f0)/(f0_hat_sd^2) -
                              (f0_hat_intercept*f0_hat_slope)/(f0_hat_sd^2))
  slope_f0_given_psi = ((-f0_hat_slope^2 / (f0_hat_sd^2)))
  f0_given_psi_mu = (-intercept_f0_given_psi/slope_f0_given_psi)
  f0_given_psi_sd = sqrt(-1/slope_f0_given_psi)
  f0_hat = f0_hat_intercept +f0_hat_slope*f0_given_psi_mu

  f0_given_psi_density = stats::dnorm (f0, f0_hat, f0_hat_sd, log=TRUE)
  psi_prior_density = stats::dnorm (PSI_prior_mean, PSI_prior_mean, PSI_prior_sd, log=TRUE)

  tmp = combine_gaussians(f0_given_psi_mu, f0_given_psi_sd,
                          PSI_prior_mean, PSI_prior_sd,
                          f0_given_psi_density, psi_prior_density)
  colnames(tmp) = c("prior_mu", "prior_sd", "prior_density")
  return(tmp)
}


# Which built-in method is this, and which of its arguments are prior
# parameters? NULL for user-supplied methods.
#' @noRd

batch_spec = function(method){
  if (identical(method, method6))
    return(list(prior = "f0", params = c("PSI_prior_mean", "PSI_prior_sd", "f0_hat_sd",
                                         "f0_hat_intercept", "f0_hat_slope")))
  if (identical(method, method3))
    return(list(prior = "fixed", params = c("PSI_prior_mean", "PSI_prior_sd")))
  if (identical(method, method2))
    return(list(prior = "none", params = character(0)))
  return(NULL)
}


# Prior parameters for a built-in method, given the arguments the method would
# be called with. A copy of the method that only returns its prior parameters
# is called with those arguments, so R matches them exactly as it would for
# the method itself (exact names, partial names, and position), and defaults
# are filled in the same way.
#' @noRd

batch_params = function(method, spec, args){
  if (length(spec$params) == 0) return(NULL)
  getter = method
  body(getter) = bquote(scalar_params(mget(.(spec$params))))
  do.call(getter, args)
}


# Named numeric vector of prior parameters, each a single unnamed number.
#' @noRd

scalar_params = function(values){
  values = lapply(values, function(x) as.numeric(unname(x)))
  if (any(lengths(values) != 1))
    stop("Prior parameters must be single numbers.", call. = FALSE)
  unlist(values)
}


# Turn the batch matrices into STM_output objects (or lite output).
#' @noRd

batch_results = function(batch, ffs, f0, template, parameters, vowel_priors, lite,
                         stimuli = NULL){
  n = nrow(batch$posterior_probability)

  if (lite){
    psi = batch$posterior_mu
    posterior = batch$posterior_probability
    rownames(psi) = rownames(posterior) = seq_len(n)
    return(list(psi = psi, posterior = posterior))
  }

  if (is.null(parameters)) parameters = NA
  classes = colnames(batch$posterior_probability)
  # k x 11 x n array, so each token's table is one contiguous slice
  values = aperm(array(unlist(batch, use.names = FALSE),
                       c(n, length(classes), length(batch))), c(2, 3, 1))
  column_names = names(batch)
  row_names = classes

  outputs = lapply(seq_len(n), function(i){
    table = values[, , i]
    if (is.null(dim(table))) table = matrix(table, nrow = 1)
    df = lapply(seq_along(column_names), function(c) table[, c])
    names(df) = column_names
    attr(df, "row.names") = row_names
    class(df) = "data.frame"

    stimulus = if (is.null(stimuli)) c(ffs[i, ], f0 = f0[i]) else stimuli[[i]]
    STM_output(df, stimulus, template, parameters, vowel_priors)
  })

  winner = max.col(batch$posterior_probability, ties.method = "first")
  winners = data.frame(psi_hat = batch$posterior_mu[cbind(seq_len(n), winner)],
                       vowel_hat = classes[winner], stringsAsFactors = FALSE)
  posterior = batch$posterior_probability
  rownames(posterior) = NULL

  list(outputs = outputs, winners = winners, posterior = STM_posteriors(posterior))
}
