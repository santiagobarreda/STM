
test_that("unrecognized arguments give a warning instead of being ignored", {
  expect_warning(BSTM(lf[1:3, ], lf0[1:3], template, PSI_prior_mena = 7.6),
                 "Unused argument.*PSI_prior_mena")
  expect_warning(method6(lf[1, ], lf0[1], template, PSI_prior_mena = 7.6),
                 "Unused argument.*PSI_prior_mena")
  expect_warning(BSTM(lf[1:3, ], lf0[1:3], template, method = method2, PSI_prior_mean = 7.6),
                 "Unused argument.*method2")
  expect_warning(estimate_f0_psi_prior(lf0[1], f0_slope = 2), "Unused argument")
  # recognized arguments, including partial names, do not warn
  expect_no_warning(BSTM(lf[1:3, ], lf0[1:3], template, PSI_prior_m = 7.5))
  expect_no_warning(BSTM(lf[1:3, ], lf0[1:3], template, method = method3, PSI_prior_sd = 0.2))
})

test_that("Hz input with a log-scale template gives a warning", {
  expect_warning(BSTM(as.matrix(formants[1:3, ]), lf0[1:3], template), "look like Hz")
  expect_warning(method6(as.numeric(formants[1, ]), lf0[1], template), "look like Hz")
  expect_warning(BSTM(lf[1:3, ], d$f0[1:3], template), "f0 values .* look like Hz")
  # no warning for log input, for f0 with user-supplied f0 parameters, or for
  # methods that do not use f0
  expect_no_warning(BSTM(lf[1:3, ], lf0[1:3], template))
  expect_no_warning(BSTM(lf[1:3, ], d$f0[1:3], template, f0_hat_intercept = 0,
                         f0_hat_slope = 30, f0_hat_sd = 20))
  expect_no_warning(BSTM(lf[1:3, ], d$f0[1:3], template, method = method3))
  # Hz data with a Hz template is the user's choice: no warning
  hz_template = create_template(as.matrix(formants), d$vowel)
  expect_no_warning(BSTM(as.matrix(formants[1:3, ]), lf0[1:3], hz_template, method = method2))
})

test_that("formant columns in a different order from the template give a warning", {
  expect_equal(template$features, c("formants.f1", "formants.f2", "formants.f3"))
  expect_warning(BSTM(lf[1:3, c(2, 1, 3)], lf0[1:3], template), "different order")
  expect_warning(method6(lf[1, c(2, 1, 3)], lf0[1], template), "different order")
  expect_warning(STM(nd[1:3, c(2, 1, 3)], template), "different order")
  # same order (with or without the "formants." prefix), or unnamed: no warning
  expect_no_warning(BSTM(lf[1:3, ], lf0[1:3], template))
  expect_no_warning(BSTM(unname(lf[1:3, ]), lf0[1:3], template))
  expect_no_warning(STM(nd[1:3, 1:3], template))
  # templates made before feature names were stored still work
  old_template = template
  old_template$features = NULL
  expect_no_warning(BSTM(lf[1:3, c(2, 1, 3)], lf0[1:3], old_template))
})

test_that("method6 needs exactly one f0 per token", {
  expect_error(method6(lf[1, ], lf0[1:2], template), "one f0 value per token")
})

test_that("model_likelihood ignores cells with no responses", {
  expect_equal(model_likelihood(rbind(c(5, 0)), rbind(c(1, 0)), correctOUflow = FALSE), 0)
  expect_equal(model_likelihood(c(5, 0), c(0.5, 0), correctOUflow = FALSE), 5 * log(0.5))
})

test_that("lobanov_normalize gives NA for tokens without a speaker ID", {
  s = d$speaker
  s[3] = NA
  out = lobanov_normalize(formants, s, d$vowel)
  expect_true(all(is.na(out[3, 1:3])))
  expect_false(anyNA(out[-3, 1:3]))
})

test_that("missing formants with a user-supplied method give NA, not an error", {
  x = rbind(lf[1, ], c(NA, lf[2, 2:3]))
  looped = BSTM(x, lf0[1:2], template, method = function(...) method6(...))
  batched = BSTM(x, lf0[1:2], template)
  expect_equal(looped$winners, batched$winners)
  expect_true(is.na(looped$winners$vowel_hat[2]))
})

test_that("plot passes arguments on to plot_posterior", {
  pdf(NULL)
  on.exit(dev.off())
  expect_no_error(plot(BSTM(lf[1, ], lf0[1], template), psi_range = c(6.5, 8)))
})

test_that("print methods return the object invisibly", {
  out = BSTM(lf[1:2, ], lf0[1:2], template)
  for (x in list(out[[1]], out, out$posterior, template)) {
    capture.output(returned <- withVisible(print(x)))
    expect_identical(returned$value, x)
    expect_false(returned$visible)
  }
})

test_that("bootstrap relabeling never merges different speakers", {
  toy = data.frame(s = c("m01", "m01_2", "x"), x = 1:3)
  set.seed(5)
  for (k in 1:200) {
    boot = bootstrap_data(toy, "s")
    # each ID should come from exactly one original row (one row per speaker here)
    expect_false(any(duplicated(boot$s)))
  }
  expect_error(bootstrap_data(data.frame(s = c("a", NA), x = 1:2), "s"), "missing values")
})

test_that("unclear errors are now clear", {
  expect_error(normalize(matrix(NA_real_, 3, 3), 1:3, 1:3), "no observed values")
  expect_error(create_template(nd[1:12, 1:3], nd$vowels[1:12], shared_covar = TRUE),
               "more tokens than classes")
  expect_error(integrate_numerical(1, 1), "at least two points")
  expect_error(integrate_numerical(1:3, 1:2), "same length")
})
