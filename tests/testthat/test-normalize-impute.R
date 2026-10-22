
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
