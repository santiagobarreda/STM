
test_that("normalize gives speaker log-means for balanced data", {
  lf_df = log(formants)
  expected = tapply(rowMeans(lf_df), d$speaker, mean)
  gbar = tapply(nd$gbar, nd$speakers, unique)
  expect_equal(as.numeric(gbar), as.numeric(expected[names(gbar)]))
  expect_equal(as.matrix(nd[, 1:3]), as.matrix(lf_df) - nd$gbar, ignore_attr = TRUE)
  expect_equal(names(nd), c("formants.f1", "formants.f2", "formants.f3",
                            "vowels", "speakers", "gbar"))
})

test_that("normalize accepts matrices and already-logged data", {
  from_matrix = normalize(as.matrix(formants), d$speaker, d$vowel)
  from_log = normalize(log(formants), d$speaker, d$vowel)
  expect_equal(from_matrix, nd)
  expect_equal(from_log, nd)
})

test_that("normalize is unaffected by unused factor levels", {
  sub = d$speaker %in% c("m01", "w01", "b01")
  spk = factor(d$speaker[sub], levels = sort(unique(d$speaker)))
  vowel_levels = c("aa", sort(unique(d$vowel)), "zz")
  base = normalize(formants[sub, ], as.character(d$speaker[sub]), d$vowel[sub])
  with_unused = normalize(formants[sub, ], spk, factor(d$vowel[sub], levels = vowel_levels))
  expect_equal(with_unused$gbar, base$gbar)
  expect_false(anyNA(with_unused$gbar))
})

test_that("normalize treats zeros as missing instead of returning -Inf", {
  f = formants
  f[1, 1] = 0
  out = normalize(f, d$speaker, d$vowel)
  expect_true(is.na(out[1, 1]))
  expect_false(any(is.infinite(as.matrix(out[, 1:3]))))
})

test_that("impute_NA returns the input shape and only fills missing values", {
  f = h95_data[, c("f1", "f2", "f3")]
  missing = is.na(f)
  expect_true(any(missing))
  imputed = impute_NA(f, h95_data$speaker, h95_data$vowel)
  expect_equal(dim(imputed), dim(f))
  expect_s3_class(imputed, "data.frame")
  expect_false(anyNA(imputed))
  expect_equal(imputed[!missing], f[!missing])

  imputed_matrix = impute_NA(as.matrix(f), h95_data$speaker, h95_data$vowel)
  expect_true(is.matrix(imputed_matrix))
  expect_equal(unname(imputed_matrix), unname(as.matrix(imputed)))
})

test_that("impute_NA recovers held-out values reasonably well", {
  f = as.matrix(formants)
  set.seed(1)
  holdout = sample(length(f), 50)
  f_missing = f
  f_missing[holdout] = NA
  imputed = impute_NA(f_missing, d$speaker, d$vowel)
  expect_lt(median(abs(log(imputed[holdout]) - log(f[holdout]))), 0.15)
})

test_that("impute_NA keeps logged data on the log scale", {
  f = log(as.matrix(formants))
  f[1, 1] = NA
  imputed = impute_NA(f, d$speaker, d$vowel)
  expect_lt(max(imputed), 20)
  expect_equal(imputed[-1], f[-1])
})

test_that("sim_error adds noise to imputed values only", {
  f = h95_data[, c("f1", "f2", "f3")]
  set.seed(1)
  a = impute_NA(f, h95_data$speaker, h95_data$vowel, sim_error = FALSE)
  set.seed(1)
  b = impute_NA(f, h95_data$speaker, h95_data$vowel, sim_error = TRUE)
  missing = is.na(f)
  expect_false(isTRUE(all.equal(a[missing], b[missing])))
  expect_equal(a[!missing], b[!missing])
})

test_that("lobanov_normalize handles one-token speakers and missing vowels", {
  out = lobanov_normalize(formants, d$speaker, d$vowel)
  expect_false(anyNA(out[, 1:3]))

  # drop one vowel for one speaker: that speaker should still be normalized
  keep = !(d$speaker == "m01" & d$vowel == "ae")
  out = lobanov_normalize(formants[keep, ], d$speaker[keep], d$vowel[keep])
  expect_false(anyNA(out[, 1:3]))

  expect_no_error(lobanov_normalize(formants[1:13, ], c(rep("a", 12), "b"), d$vowel[1:13]))
})

# Reference fit with lm(), as normalize_impute() did before the fast solver
lm_reference = function(f, speakers, vowels){
  y = c(log(as.matrix(f)))
  df = data.frame(y = y, s = factor(rep(speakers, ncol(f))),
                  vk = interaction(factor(rep(vowels, ncol(f))), rep(seq_len(ncol(f)), each = nrow(f))))
  df = droplevels(df[!is.na(df$y), ])
  stats::contrasts(df$vk) = "contr.sum"
  model = stats::lm(y ~ 0 + s + vk, data = df)
  list(model = model, gbar = stats::coef(model)[paste0("s", levels(df$s))])
}

test_that("fast solver matches lm() on balanced and unbalanced data", {
  # complete, balanced data
  ref = lm_reference(formants, d$speaker, d$vowel)
  gbar = tapply(nd$gbar, nd$speakers, unique)
  expect_equal(as.numeric(gbar), unname(ref$gbar), tolerance = 1e-12)

  # data with missing values
  f = h95_data[, c("f1", "f2", "f3")]
  ref = lm_reference(f, h95_data$speaker, h95_data$vowel)
  out = normalize(f, h95_data$speaker, h95_data$vowel)
  expect_equal(as.numeric(tapply(out$gbar, out$speakers, unique)), unname(ref$gbar),
               tolerance = 1e-12)

  # unbalanced: drop a random 30% of tokens
  set.seed(4)
  keep = runif(nrow(d)) > 0.3
  ref = lm_reference(formants[keep, ], d$speaker[keep], d$vowel[keep])
  out = normalize(formants[keep, ], d$speaker[keep], d$vowel[keep])
  expect_equal(as.numeric(tapply(out$gbar, droplevels(out$speakers), unique)),
               unname(ref$gbar), tolerance = 1e-12)
})

test_that("fast imputation matches lm() predictions and residual sd", {
  f = h95_data[, c("f1", "f2", "f3")]
  missing = is.na(f)
  ref = lm_reference(f, h95_data$speaker, h95_data$vowel)
  newdata = data.frame(s = factor(rep(h95_data$speaker, 3)),
                       vk = interaction(factor(rep(h95_data$vowel, 3)), rep(1:3, each = nrow(f))))
  newdata = newdata[c(missing), ]
  expected = round(exp(stats::predict(ref$model, newdata = newdata)))
  imputed = impute_NA(f, h95_data$speaker, h95_data$vowel)
  expect_equal(unname(imputed[missing]), unname(expected))

  fit_data = data.frame(ffs = c(log(as.matrix(f))), speakers_x = factor(rep(h95_data$speaker, 3)),
                        vk = interaction(factor(rep(h95_data$vowel, 3)), rep(1:3, each = nrow(f))))
  fit_data = droplevels(fit_data[!is.na(fit_data$ffs), ])
  fit = fit_speaker_cell(fit_data$ffs, fit_data$speakers_x, fit_data$vk)
  expect_equal(fit$sigma, stats::sigma(ref$model), tolerance = 1e-10)
  lm_fit = fit_speaker_cell_lm(fit_data)
  expect_equal(fit$speaker, lm_fit$speaker, tolerance = 1e-10)
  expect_equal(fit$cell, lm_fit$cell, tolerance = 1e-10)
})

test_that("disconnected designs fall back to lm()", {
  # two groups of speakers that share no vowels
  sub = d[d$speaker %in% c("m01", "m02", "w01", "w02"), ]
  group_a = sub$speaker %in% c("m01", "m02") & sub$vowel %in% c("ae", "ah", "aw")
  group_b = sub$speaker %in% c("w01", "w02") & sub$vowel %in% c("iy", "uw", "oo")
  sub = sub[group_a | group_b, ]
  fit_data = data.frame(ffs = c(log(as.matrix(sub[, c("f1", "f2", "f3")]))),
                        speakers_x = factor(rep(sub$speaker, 3)),
                        vk = interaction(factor(rep(sub$vowel, 3)), rep(1:3, each = nrow(sub))))
  fit_data = droplevels(fit_data)
  expect_null(fit_speaker_cell(fit_data$ffs, fit_data$speakers_x, fit_data$vk))
  expect_warning(normalize(sub[, c("f1", "f2", "f3")], sub$speaker, sub$vowel), "rank deficient")
})

test_that("rows with missing speaker or vowel IDs are left out, as lm() did", {
  s = d$speaker; v = d$vowel
  s[c(5, 40)] = NA
  v[c(9, 300)] = NA
  out = normalize(formants, s, v)
  complete = !is.na(s) & !is.na(v)
  ref = lm_reference(formants[complete, ], s[complete], v[complete])
  gbar = tapply(out$gbar, out$speakers, function(x) unique(x[!is.na(x)]))
  expect_equal(as.numeric(gbar), unname(ref$gbar), tolerance = 1e-12)
  expect_true(all(is.na(out$gbar[is.na(s)])))

  # imputation: fine when the missing formant has known IDs, an error otherwise
  f = formants
  f[100, 1] = NA
  expect_false(anyNA(impute_NA(f, s, v)))
  f[5, 1] = NA
  expect_error(impute_NA(f, s, v), "missing speaker or vowel ID")
})
