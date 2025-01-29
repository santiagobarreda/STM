

#' @keywords internal

normalize_impute = function (formants, speakers, vowels, normalize = TRUE,
                             impute = TRUE, sim_error = FALSE){

  nffs = ncol(formants)
  n = nrow (formants)
  ffs = c(unlist(formants))
  ffs[ffs==0] = NA

  if (max(ffs, na.rm = TRUE) > 20){
    ffs = log (ffs)
    formants = log(formants)
  }

  fnum = as.factor (rep(1:nffs, each = n))
  speakers_x = as.factor(rep(speakers,nffs))
  vowels_x = as.factor(rep(vowels,nffs))
  vk = interaction (vowels_x, fnum)
  nspeakers = length(unique((speakers)))
  nvowels = length(unique((vowels)))

  stats::contrasts (vk) = 'contr.sum'
  stats::contrasts (speakers_x) = 'contr.sum'

  model = stats::lm (ffs ~ 0+speakers_x + vk)
  gbars = model$coefficients[1:nspeakers]

  if (impute){
    missing = data.frame (ffs, vk, speakers_x)[is.na(ffs),]
    if (nrow(missing) > 0){
      suppressWarnings({
        ffs[is.na(ffs)] = stats::predict(model, newdata = missing)
      })
      if (sim_error)
        ffs[is.na(ffs)] = ffs[is.na(ffs)] + stats::rnorm(nrow(missing), 0, sd(model$residuals))
    }
    output = matrix (round(exp(ffs)), nffs, n)
  }

  if (normalize){
    output = suppressWarnings({
      cbind(formants=formants, vowels = factor(vowels),
            speakers = factor(speakers))
    })
    for (i in 1:nspeakers){
      output[levels(speakers)[i]==output$speakers,1:nffs] =
        output[levels(speakers)[i]==output$speakers,1:nffs] - gbars[i]
      output$gbar[levels(speakers)[i]==output$speakers] = gbars[i]
    }
  }

  return (output)
}
