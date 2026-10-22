
test_that("create_template computes class means and covariances", {
  ae = as.matrix(nd[nd$vowels == "ae", 1:3])
  expect_equal(unname(template$means["ae", ]), unname(colMeans(ae)))
  expect_equal(unname(template$covariance[["ae"]]), unname(stats::var(ae)))
  expect_equal(template$precision[["ae"]] %*% template$covariance[["ae"]], diag(3),
               ignore_attr = TRUE)
  expect_equal(template$log_det[["ae"]], log(det(template$covariance[["ae"]])))
})

test_that("pooled covariance uses n - k degrees of freedom", {
  pooled = create_template(nd[, 1:3], nd$vowels, shared_covar = TRUE)
  centered = as.matrix(nd[, 1:3]) - template$means[as.character(nd$vowels), ]
  expected = crossprod(centered) / (nrow(nd) - 12)
  expect_equal(unname(pooled$covariance[[1]]), unname(expected))
  expect_equal(pooled$covariance[[1]], pooled$covariance[[12]])
})

test_that("create_template ignores unused factor levels", {
  classes = factor(nd$vowels, levels = c(levels(nd$vowels), "zz"))
  t2 = create_template(nd[, 1:3], classes)
  expect_equal(t2$classes, template$classes)
  expect_equal(t2$means, template$means)
})

test_that("single-token classes give a clear error, or work with a shared covariance", {
  classes = c(as.character(nd$vowels[1:24]), "zz")
  expect_error(create_template(nd[1:25, 1:3], classes), "at least 2 tokens")
  expect_no_error(create_template(nd[1:25, 1:3], classes, shared_covar = TRUE))
})

test_that("STM returns STM_posteriors that work with apply, nrow and [", {
  post = STM(nd[1:10, 1:3], template)
  expect_s3_class(post, "STM_posteriors")
  expect_equal(dim(post), c(10, 12))
  expect_equal(nrow(post), 10)
  expect_equal(apply(post, 1, sum), rep(1, 10), ignore_attr = TRUE)
  expect_equal(post[2, ], post$df[2, ])
  expect_equal(post[1:2, "ae", drop = FALSE], post$df[1:2, "ae", drop = FALSE])
  expect_equal(colnames(post), template$classes)
})

test_that("STM gives the same answers whether or not the precision is cached", {
  old_template = template
  old_template$precision = NULL
  old_template$log_det = NULL
  expect_equal(STM(nd[1:20, 1:3], old_template)$df, STM(nd[1:20, 1:3], template)$df)
})

test_that("plots run without error", {
  pdf(NULL)
  on.exit(dev.off())
  old_mfrow = par("mfrow")
  expect_no_error(plot(template))
  expect_no_error(plot(BSTM(lf[1, ], lf0[1], template)))
  expect_no_error(plot(BSTM(lf[1, ], lf0[1], template, method = method2)))
  expect_equal(par("mfrow"), old_mfrow)
})
