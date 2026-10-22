

# Shared engine for normalize() and impute_NA(). Fits
# log(formant) ~ 0 + speaker + vowel:formant (sum-to-zero contrasts on the
# vowel:formant effects), so each speaker coefficient is that speaker's
# log-mean formant frequency (Barreda & Nearey, 2018).
#' @keywords internal
#' @noRd

normalize_impute = function (formants, speakers, vowels, normalize = TRUE,
                             impute = FALSE, sim_error = FALSE){

  if (normalize == impute)
    stop("Exactly one of 'normalize' or 'impute' must be TRUE.", call. = FALSE)

  input_is_df = is.data.frame(formants)
  formants = as.matrix(formants)
  if (!is.numeric(formants)) stop("formants must be numeric.", call. = FALSE)
  if (is.null(colnames(formants)))
    colnames(formants) = paste0("f", seq_len(ncol(formants)))

  nffs = ncol(formants)
  n = nrow (formants)
  if (length(speakers) != n || length(vowels) != n)
    stop("speakers and vowels must have one element per row of formants.", call. = FALSE)

  # Zeros are treated as missing values
  formants[which(formants == 0)] = NA
  input_formants = formants

  logged = max(formants, na.rm = TRUE) <= 20
  if (!logged) formants = log(formants)

  # Unused factor levels would make the model rank deficient, which silently
  # changes the constraint on the vowel effects and misaligns speaker estimates
  speakers = droplevels(as.factor(speakers))
  vowels = droplevels(as.factor(vowels))

  ffs = c(formants)
  fnum = factor(rep(seq_len(nffs), each = n))
  model_data = data.frame(ffs = ffs, speakers_x = rep(speakers, nffs),
                          vk = interaction(rep(vowels, nffs), fnum))
  observed = !is.na(ffs)
  fit_data = droplevels(model_data[observed, ])

  fit = fit_speaker_cell(fit_data$ffs, fit_data$speakers_x, fit_data$vk)
  if (is.null(fit)) fit = fit_speaker_cell_lm(fit_data)
  gbars = fit$speaker[levels(speakers)]
  names(gbars) = levels(speakers)

  if (impute){
    missing_idx = which(!observed)
    output = input_formants
    if (length(missing_idx) > 0){
      missing = model_data[missing_idx, ]
      if (!all(missing$vk %in% levels(fit_data$vk)) ||
          !all(missing$speakers_x %in% levels(fit_data$speakers_x)))
        stop("Cannot impute: some speaker or vowel/formant combination has no observed data.",
             call. = FALSE)
      predicted = unname(fit$speaker[as.character(missing$speakers_x)] +
                           fit$cell[as.character(missing$vk)])
      if (sim_error)
        predicted = predicted + stats::rnorm(length(missing_idx), 0, fit$sigma)

      output[missing_idx] = if (logged) predicted else round(exp(predicted))
    }
    if (input_is_df) output = as.data.frame(output)
    return (output)
  }

  speaker_gbar = unname(gbars[as.character(speakers)])
  normalized = formants - speaker_gbar
  colnames(normalized) = paste0("formants.", colnames(formants))
  output = data.frame(normalized, vowels = vowels, speakers = speakers,
                      gbar = speaker_gbar)
  return (output)
}


# Least-squares fit of y ~ 0 + speaker + cell, with sum-to-zero cell effects
# (equivalent to lm() with contr.sum on the cells). The speaker effects are
# eliminated from the normal equations, leaving a small system in the cell
# effects only. Returns NULL if that system is singular (a disconnected
# design), in which case fit_speaker_cell_lm() should be used.
#' @noRd

fit_speaker_cell = function(y, speaker, cell){
  n_speakers = nlevels(speaker)
  n_cells = nlevels(cell)
  s = as.integer(speaker)
  k = as.integer(cell)

  counts = matrix(tabulate(s + n_speakers * (k - 1L), n_speakers * n_cells),
                  n_speakers, n_cells)
  n_s = rowSums(counts)
  n_k = colSums(counts)
  sum_s = as.vector(rowsum(y, s, reorder = TRUE))
  sum_k = as.vector(rowsum(y, k, reorder = TRUE))

  # reduced normal equations A b = rhs; A is singular along the all-ones
  # vector, so A + 1 (adding a matrix of ones) gives the solution with sum(b) = 0
  A = diag(n_k, n_cells) - crossprod(counts, counts / n_s)
  rhs = sum_k - drop(crossprod(counts, sum_s / n_s))
  A1 = A + 1
  if (rcond(A1) < 1e-10) return(NULL)
  cell_effects = drop(solve(A1, rhs))
  cell_effects = cell_effects - mean(cell_effects)
  speaker_effects = (sum_s - drop(counts %*% cell_effects)) / n_s

  residuals = y - speaker_effects[s] - cell_effects[k]
  df = length(y) - (n_speakers + n_cells - 1)
  list(speaker = stats::setNames(speaker_effects, levels(speaker)),
       cell = stats::setNames(cell_effects, levels(cell)),
       sigma = sqrt(sum(residuals^2) / df))
}


# Same fit using lm(), for designs fit_speaker_cell() cannot handle.
#' @noRd

fit_speaker_cell_lm = function(fit_data){
  stats::contrasts(fit_data$vk) = "contr.sum"
  model = stats::lm (ffs ~ 0 + speakers_x + vk, data = fit_data)
  if (anyNA(stats::coef(model)))
    warning("The normalization model is rank deficient; some estimates are not identifiable.",
            call. = FALSE)
  coefs = stats::coef(model)
  speaker = coefs[paste0("speakers_x", levels(fit_data$speakers_x))]
  names(speaker) = levels(fit_data$speakers_x)
  # cell effects under the sum-to-zero contrasts, including the last level
  cell = drop(stats::contrasts(fit_data$vk) %*% coefs[grep("^vk", names(coefs))])
  names(cell) = levels(fit_data$vk)
  list(speaker = speaker, cell = cell, sigma = stats::sigma(model))
}
