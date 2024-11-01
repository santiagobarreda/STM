
dlike = function (x,mean, sd, max_log_density, log = FALSE){

  if (!is.null(x[1])){
    density_ = -(x-mean)^2/(2*sd^2) + max_log_density
    if (!log) density_ = exp(density_)
  }

  if (is.null(x[1])){
    x = seq (mean-3*sd, mean+3*sd, length.out = 1000)
    density_ = -(x-mean)^2/(2*sd^2) + max_log_density
    if (!log) density_ = exp(density_)
    density_ = cbind(x, density_)
  }

  return(density_)
}
