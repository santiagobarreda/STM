

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
  speakers = as.factor(rep(speakers,nffs))
  vowels = as.factor(rep(vowels,nffs))
  vk = interaction (vowels, fnum)
  nspeakers = length(unique((speakers)))
  nvowels = length(unique((vowels)))

  stats::contrasts (vk) = 'contr.sum'
  stats::contrasts (speakers) = 'contr.sum'

  model = stats::lm (ffs ~ 0+speakers + vk)
  gbars = model$coefficients[1:nspeakers]

  if (impute){
    missing = data.frame (ffs, vk, speakers)[is.na(ffs),]
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
    output$gbar = 0
    for (i in 1:nspeakers){
      output[levels(speakers)[i]==output$speakers,1:nffs] =
        output[levels(speakers)[i]==output$speakers,1:nffs] - gbars[i]
      output$gbar[levels(speakers)[i]==output$speakers] = gbars[i]
    }
  }

  return (output)
}

# formants = h95data[,c(10:12,28:30)]
# speakers = h95data$speaker
# vowels = h95data$vowel
