
# Brute-force reference: integrate (BSTM) or maximize (PSTM) the joint density
# of formants, f0 and psi over a fine psi grid, using method6's default priors.
brute_force = function(i) {
  grid = seq(6.5, 8, length.out = 20001)
  log_joint = sapply(seq_along(template$classes), function(j) {
    shifted = outer(grid, rep(1, 3)) +
      matrix(template$means[j, ], length(grid), 3, byrow = TRUE)
    dmvnorm_fast(shifted, lf[i, ], template$covariance[[j]], log = TRUE) +
      dnorm(lf0[i], -10.32 + 2.145 * grid, 0.1327, log = TRUE) +
      dnorm(grid, 7.233, 0.1284, log = TRUE)
  })
  integral = colSums(exp(log_joint - max(log_joint)))
  peak = exp(apply(log_joint, 2, max) - max(log_joint))
  list(bstm = integral / sum(integral), pstm = peak / sum(peak),
       psi = grid[apply(log_joint, 2, which.max)])
}

test_that("BSTM and PSTM match brute-force numerical results", {
  for (i in c(1, 700, 1600)) {
    reference = brute_force(i)
    b = BSTM(lf[i, ], lf0[i], template)$df
    p = PSTM(lf[i, ], lf0[i], template)$df
    expect_equal(b$posterior_probability, reference$bstm, tolerance = 1e-8)
    expect_equal(p$posterior_probability, reference$pstm, tolerance = 1e-5)
    expect_equal(b$posterior_mu, reference$psi, tolerance = 1e-4)
  }
})

test_that("method6 can be called directly without vowel_priors", {
  out = method6(lf[1, ], lf0[1], template)
  expect_s3_class(out, "STM_output")
  expect_equal(sum(out$df$posterior_probability), 1)
})

test_that("vowel_priors work with every method and shift posteriors", {
  priors = setNames(rep(1, 12), template$classes)
  priors["ae"] = 100
  for (method in list(method6, method3, method2)) {
    flat = BSTM(lf[1, ], lf0[1], template, method = method)$df
    tilted = BSTM(lf[1, ], lf0[1], template, method = method,
                  vowel_priors = priors)$df
    expect_equal(dim(tilted), dim(flat))
    expect_true(tilted["ae", "posterior_probability"] >
                  flat["ae", "posterior_probability"])
    expect_equal(sum(tilted$posterior_probability), 1)
  }
})

test_that("uniform vowel_priors leave posteriors unchanged", {
  flat = BSTM(lf[1:3, ], lf0[1:3], template)$posterior$df
  uniform = BSTM(lf[1:3, ], lf0[1:3], template,
                 vowel_priors = rep(1 / 12, 12))$posterior$df
  expect_equal(uniform, flat)
})

test_that("named vowel_priors are reordered to match the template", {
  priors = setNames(seq_len(12), template$classes)
  a = STM(nd[1:3, 1:3], template, vowel_priors = priors)
  b = STM(nd[1:3, 1:3], template, vowel_priors = rev(priors))
  expect_equal(a$df, b$df)
})

test_that("bad vowel_priors are rejected", {
  expect_error(BSTM(lf[1, ], lf0[1], template, vowel_priors = rep(1, 3)), "one value")
  expect_error(STM(nd[1, 1:3], template, vowel_priors = setNames(rep(1, 12), letters[1:12])),
               "names")
  expect_error(STM(nd[1, 1:3], template, vowel_priors = rep(-1, 12)), "non-negative")
})

test_that("lite output works with every method and matches full output", {
  for (method in list(method6, method3, method2)) {
    lite = BSTM(lf[1:3, ], lf0[1:3], template, method = method, lite = TRUE)
    full = BSTM(lf[1:3, ], lf0[1:3], template, method = method)
    expect_equal(dim(lite$posterior), c(3, 12))
    expect_equal(unname(lite$posterior), unname(full$posterior$df))
    expect_equal(unname(lite$psi[cbind(1:3, max.col(lite$posterior))]),
                 full$winners$psi_hat)
  }
})

test_that("single tokens return STM_output for every input type", {
  expect_s3_class(BSTM(lf[1, ], lf0[1], template), "STM_output")
  expect_s3_class(BSTM(lf[1, , drop = FALSE], lf0[1], template), "STM_output")
  expect_s3_class(BSTM(as.data.frame(lf)[1, ], lf0[1], template), "STM_output")
  expect_s3_class(BSTM(lf[1:2, ], lf0[1:2], template), "STM_output_list")
})

test_that("rounded_pp stays in sync with posterior_probability", {
  out = BSTM(lf[1, ], lf0[1], template)$df
  expect_equal(out$rounded_pp, round(out$posterior_probability, 4))
})

test_that("method2 and method3 store f0", {
  expect_equal(unname(method2(lf[1, ], lf0[1], template)$stimulus["f0"]), lf0[1])
  expect_equal(unname(method3(lf[1, ], lf0[1], template)$stimulus["f0"]), lf0[1])
})

test_that("invalid type is an error, and PSTM ignores a user-supplied type", {
  expect_error(method6(lf[1, ], lf0[1], template, type = "bstm"))
  expect_error(scale_posterior(cbind(7, 0.1, -1, 0, 0), type = "bstm"))
  expect_warning(out <- PSTM(lf[1, ], lf0[1], template, type = "BSTM"), "ignored")
  expect_equal(out$df, PSTM(lf[1, ], lf0[1], template)$df)
})

test_that("cached precision gives the same likelihoods as the covariance alone", {
  ffs = lf[1, ]
  for (j in 1:3) {
    expect_equal(
      estimate_likelihood(ffs, template$means[j, ], template$covariance[[j]]),
      estimate_likelihood(ffs, template$means[j, ], template$covariance[[j]],
                          template$precision[[j]], template$log_det[[j]]))
  }
})

test_that("templates without cached log determinants still work", {
  old_template = template
  old_template$log_det = NULL
  expect_equal(BSTM(lf[1, ], lf0[1], old_template)$df, BSTM(lf[1, ], lf0[1], template)$df)
  expect_equal(STM(nd[1:5, 1:3], old_template)$df, STM(nd[1:5, 1:3], template)$df)
})
