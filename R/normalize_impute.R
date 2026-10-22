

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
  stats::contrasts(fit_data$vk) = "contr.sum"

  model = stats::lm (ffs ~ 0 + speakers_x + vk, data = fit_data)
  if (anyNA(stats::coef(model)))
    warning("The normalization model is rank deficient; some estimates are not identifiable.",
            call. = FALSE)
  gbars = stats::coef(model)[paste0("speakers_x", levels(speakers))]
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
      missing$vk = factor(missing$vk, levels = levels(fit_data$vk))
      missing$speakers_x = factor(missing$speakers_x, levels = levels(fit_data$speakers_x))

      predicted = stats::predict(model, newdata = missing)
      if (sim_error)
        predicted = predicted + stats::rnorm(length(missing_idx), 0, stats::sigma(model))

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
