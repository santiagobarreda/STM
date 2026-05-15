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
#' @return A template object.
#' @examples
#' # TBD
#' @export


create_template = function (features, classes, shared_covar = FALSE){

  invert_covariance = function(covariance_matrix) {
    chol_factor = tryCatch(
      chol(covariance_matrix),
      error = function(e) {
        stop("Covariance matrix must be positive definite.", call. = FALSE)
      }
    )

    list(covariance = covariance_matrix, precision = chol2inv(chol_factor))
  }

  vs = levels(as.factor(classes))
  nvs = length(vs)
  means = matrix(0, nvs, ncol(features))
  for (i in seq_len(ncol(features))) means[, i] = tapply(features[,
                                                           i], classes, mean)
  rownames(means) = vs
  colnames(means) = paste("f", seq_len(ncol(features)), sep = "")
  tmp = features
  covariance = list()
  precision = list()
  for (i in 1:nvs){
    tmp[classes == vs[i],] = features[classes == vs[i], ] -
      matrix(means[i, ], nrow(tmp[classes == vs[i],]), ncol(means), byrow = TRUE)
    if (!shared_covar){
      covariance_info = invert_covariance(stats::var(tmp[classes == vs[i],]))
      covariance[[i]] = covariance_info$covariance
      precision[[i]] = covariance_info$precision
    }
  }
  if (shared_covar){
    covariance_info = invert_covariance(stats::var(tmp))
    covar = covariance_info$covariance
    precis = covariance_info$precision

    for (i in 1:nvs) covariance[[i]] = covar
    for (i in 1:nvs) precision[[i]] = precis

  }

  ranges = matrix(0, ncol(features), 2)
  for (i in seq_len(ncol(features))) ranges[i, ] = range(features[, i])

  output = list(classes = vs, means = means, covariance = covariance,
                precision = precision, ranges = ranges)
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

  cols = rep(c("#F7B5C5BF", "#27C0D8BF", "#F8A61BBF", "#0C8275BF", "#DB686FDC",
           "#822B32BF", "#3A65AFBF", "#FF6400BF", "#413069B4", "#94CB94C8",
           "#FEEF94C8", "#88021BB4", "#719F9FDC", "#CA87B9BF", "#6C0F2DC8"),10)

  ellipses_1 = list()
  ellipses_2 = list()
  min_x = 1000000
  max_x = -100000
  min_y = 1000000
  max_y = -100000
  for (i in seq_len(nrow(x$means))){
    ellipses_1[[i]] = draw_ellipse(x$means[i, 2:1], x$covariance[[i]][2:1, 2:1], show=FALSE)
    ellipses_2[[i]] = draw_ellipse(x$means[i, 2:1], x$covariance[[i]][2:1, 2:1]*2, show=FALSE)
    if (min(ellipses_2[[i]][,1]) < min_x) min_x = min(ellipses_2[[i]][,1])
    if (max(ellipses_2[[i]][,1]) > max_x) max_x = max(ellipses_2[[i]][,1])
    if (min(ellipses_2[[i]][,2]) < min_y) min_y = min(ellipses_2[[i]][,2])
    if (max(ellipses_2[[i]][,2]) > max_y) max_y = max(ellipses_2[[i]][,2])
  }

  plot(x$means[, 2:1], type = "n", xlab = "F1", ylab = "F2",
       xlim = c(max_x,min_x), ylim = c(max_y,min_y))
  #rect (par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey60", border = NA)
  for (i in seq_len(nrow(x$means))){
    lines(ellipses_1[[i]], type = "l", col = cols[i], lwd=2)
    lines(ellipses_2[[i]], type = "l", col = cols[i])
  }
  text (x$means[, 2], x$means[, 1], labels = x$classes, col = cols, cex = 2.5)


}
