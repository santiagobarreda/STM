
test_that("bootstrap_data samples speakers, not rows", {
  toy = data.frame(s = c(rep("A", 98), "B", "C"), x = 1)
  set.seed(1)
  all_a = replicate(2000, all(bootstrap_data(toy, "s", relabel = FALSE)$s == "A"))
  expect_lt(mean(all_a), 0.1)   # expected (1/3)^3 = 0.037
})

test_that("bootstrap_data keeps whole speakers and handles numeric IDs", {
  toy = data.frame(s = c(101, 101, 205, 307, 307, 307), x = 1:6)
  set.seed(2)
  for (k in 1:20) {
    boot = bootstrap_data(toy, "s", relabel = FALSE)
    expect_true(nrow(boot) >= 3)
    counts = table(boot$s)
    original = table(toy$s)
    expect_true(all(counts %% original[names(counts)] == 0))
  }
})

test_that("bootstrap_data relabels repeated speakers", {
  toy = data.frame(s = c("A", "A", "B"), x = 1:3)
  set.seed(3)
  for (k in 1:20) {
    boot = bootstrap_data(toy, "s")
    # every draw gets its own ID, and each ID keeps all of its speaker's rows
    expect_equal(length(unique(boot$s)), 2)
    original = sub("_[0-9]+$", "", boot$s)
    expect_equal(as.vector(table(boot$s)[unique(boot$s)]),
                 as.vector(c(A = 2, B = 1)[original[!duplicated(boot$s)]]))
  }
  expect_error(bootstrap_data(toy), "speaker")
  expect_error(bootstrap_data(toy, toy$s), "speaker")
})

test_that("model_likelihood accepts STM_posteriors and matrices", {
  post = STM(nd[, 1:3], template)
  expected = sum(as.matrix(h95_classifications) * log(post$df))
  expect_equal(model_likelihood(h95_classifications, post), expected)
  expect_equal(model_likelihood(h95_classifications, post$df), expected)
  expect_equal(model_likelihood(h95_classifications, post$df[, 12:1]), expected)
})

test_that("model_likelihood does not rescale vector input", {
  post = STM(nd[, 1:3], template)
  expect_equal(model_likelihood(h95_classifications[, 1], post$df[, 1]),
               sum(h95_classifications[, 1] * log(post$df[, 1])))
  expect_error(model_likelihood(1:3, c(0.5, 0.5)), "length")
  expect_error(model_likelihood(h95_classifications[1:2, ], post$df), "dimensions")
})

test_that("correctOUflow_internal rescales vectors and matrix rows", {
  expect_equal(sum(correctOUflow_internal(c(0, 0.4, 0.6))), 1)
  m = correctOUflow_internal(rbind(c(0, 0.4, 0.6), c(0.2, 0.2, 0.2)))
  expect_equal(rowSums(m), c(1, 1))
  expect_equal(m[2, ], rep(1 / 3, 3))
  expect_true(all(m > 0))
})

test_that("combine_gaussians default densities are log densities", {
  out = combine_gaussians(0, 1, 0, 1)
  expect_equal(unname(out[1, "d"]), 2 * dnorm(0, log = TRUE))
  # the product of two N(0,1) densities, evaluated numerically
  x = seq(-5, 5, length.out = 1e5)
  expect_equal(exp(unname(out[1, "d"])), max(dnorm(x)^2), tolerance = 1e-6)
})

test_that("STM_output_list subsetting keeps the class and attributes", {
  res = BSTM(lf[1:4, ], lf0[1:4], template)
  sub = res[2:3]
  expect_s3_class(sub, "STM_output_list")
  expect_equal(length(sub), 2)
  expect_equal(sub$winners, res$winners[2:3, ], ignore_attr = TRUE)
  expect_equal(get_winners(res$list), res$winners)
  expect_error(get_winners(list(1)), "STM_output")
})

test_that("STM_output subsetting follows data frame semantics", {
  out = BSTM(lf[1, ], lf0[1], template)
  expect_equal(out["posterior_probability"], out$df["posterior_probability"])
  expect_equal(out[1:2, "posterior_mu"], out$df[1:2, "posterior_mu"])
  expect_equal(out[1, ], out$df[1, ])
})
