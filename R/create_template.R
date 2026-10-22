#' Create a Template
#'
#' Create a dialectal template for use with the (X)STM family of functions.
#'
#' @param features A matrix or data frame of acoustic information. Each row is a
#' different observation, each column is a formant measurement, f0, duration,
#' or any other token characteristic.
#' @param classes A vector of class labels for each observation.
#' @param shared_covar A boolean indicating whether to use a shared covariance
#' matrix for all classes.
#' @return A template object: a list with the class labels, class means,
#' covariance and precision matrices, covariance log determinants, and the
#' range of each feature.
#' @examples
#' \dontrun{
#' d = h95_data_no_NA
#' nd = normalize(d[, c("f1", "f2", "f3")], d$speaker, d$vowel)
#' template = create_template(nd[, 1:3], nd$vowels)
#' template
#' plot(template)
#' }
#' @export


create_template = function (features, classes, shared_covar = FALSE){

  invert_covariance = function(covariance_matrix) {
    chol_factor = tryCatch(
      chol(covariance_matrix),
      error = function(e) {
        stop("Covariance matrix must be positive definite.", call. = FALSE)
      }
    )

    list(precision = chol2inv(chol_factor),
         log_det = 2 * sum(log(diag(chol_factor))))
  }

  features = as.matrix(features)
  if (!is.numeric(features)) stop("features must be numeric.", call. = FALSE)
  if (length(classes) != nrow(features))
    stop("length(classes) must equal nrow(features).", call. = FALSE)
  if (anyNA(features) || anyNA(classes))
    stop("features and classes must not contain missing values.", call. = FALSE)

  # Unused factor levels would otherwise produce empty classes
  classes = droplevels(as.factor(classes))
  vs = levels(classes)
  nvs = length(vs)

  means = do.call(rbind, lapply(vs, function(v)
    colMeans(features[classes == v, , drop = FALSE])))
  dimnames(means) = list(vs, paste("f", seq_len(ncol(features)), sep = ""))

  if (shared_covar){
    # Pooled within-class covariance, with n - k degrees of freedom
    centered = features - means[as.integer(classes), , drop = FALSE]
    pooled = crossprod(centered) / (nrow(features) - nvs)
    covariance = rep(list(pooled), nvs)
  } else {
    counts = table(classes)
    if (any(counts < 2))
      stop("Each class needs at least 2 tokens to estimate its covariance (",
           paste(names(counts)[counts < 2], collapse = ", "),
           "). Use shared_covar = TRUE or remove these classes.", call. = FALSE)
    covariance = lapply(vs, function(v)
      stats::var(features[classes == v, , drop = FALSE]))
  }
  names(covariance) = vs

  inverses = lapply(covariance, invert_covariance)
  precision = lapply(inverses, function(x) x$precision)
  log_det = lapply(inverses, function(x) x$log_det)

  ranges = t(apply(features, 2, range))
  dimnames(ranges) = NULL

  output = list(classes = vs, means = means, covariance = covariance,
                precision = precision, log_det = log_det, ranges = ranges)
  class(output) = "STM_template"
  return(output)
}


#' @rdname create_template
#' @export
#' @method print STM_template
#' @param x A STM_template object.
#' @param ... Additional arguments.
#' @return NULL
#'

print.STM_template = function (x, ...){
  cat("\nTemplate with the following category-means:", "\n\n")
  print(x$means)
}

#' @rdname create_template
#' @export
#' @method plot STM_template
#' @param x A template object.
#' @param ... Additional arguments.
#' @return NULL
#'

plot.STM_template = function (x, ...){

  if (ncol(x$means) < 2) stop("Plotting requires at least two formants.", call. = FALSE)
  cols = rep_len(c("#F7B5C5BF", "#27C0D8BF", "#F8A61BBF", "#0C8275BF", "#DB686FDC",
           "#822B32BF", "#3A65AFBF", "#FF6400BF", "#413069B4", "#94CB94C8",
           "#FEEF94C8", "#88021BB4", "#719F9FDC", "#CA87B9BF", "#6C0F2DC8"),
           nrow(x$means))

  ellipses_1 = list()
  ellipses_2 = list()
  min_x = 1000000
  max_x = -100000
  min_y = 1000000
  max_y = -100000
  # Inner ellipses enclose 1 standard deviation, outer ellipses 2.
  for (i in seq_len(nrow(x$means))){
    ellipses_1[[i]] = draw_ellipse(x$means[i, 2:1], x$covariance[[i]][2:1, 2:1], show=FALSE)
    ellipses_2[[i]] = draw_ellipse(x$means[i, 2:1], x$covariance[[i]][2:1, 2:1]*4, show=FALSE)
    if (min(ellipses_2[[i]][,1]) < min_x) min_x = min(ellipses_2[[i]][,1])
    if (max(ellipses_2[[i]][,1]) > max_x) max_x = max(ellipses_2[[i]][,1])
    if (min(ellipses_2[[i]][,2]) < min_y) min_y = min(ellipses_2[[i]][,2])
    if (max(ellipses_2[[i]][,2]) > max_y) max_y = max(ellipses_2[[i]][,2])
  }

  plot(x$means[, 2:1], type = "n", xlab = "F2", ylab = "F1",
       xlim = c(max_x,min_x), ylim = c(max_y,min_y))
  #rect (par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey60", border = NA)
  for (i in seq_len(nrow(x$means))){
    lines(ellipses_1[[i]], type = "l", col = cols[i], lwd=2)
    lines(ellipses_2[[i]], type = "l", col = cols[i])
  }
  text (x$means[, 2], x$means[, 1], labels = x$classes, col = cols, cex = 2.5)


}
