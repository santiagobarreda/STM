
#' Function to combine two Gaussian distributions
#'
#' This function combines two Gaussian distributions by taking the mean and
#' standard deviation of the two distributions and returning a new Gaussian
#' distribution that is the combination of the two.
#'
#' @param mu_1 The mean of the first Gaussian distribution
#' @param sd_1 The standard deviation of the first Gaussian distribution
#' @param mu_2 The mean of the second Gaussian distribution
#' @param sd_2 The standard deviation of the second Gaussian distribution
#' @param d_1 The peak density of the first gaussian
#' @param d_2 The peak density of the second gaussian
#' @return The mean and standard deviation of the combined Gaussian distribution.
#' @examples
#' # TBD
#' @export
#' @importFrom stats sd


combine_gaussians = function(mu_1, sd_1, mu_2, sd_2, d_1 = NULL, d_2 = NULL){

  if (is.null(d_1)) d_1 = 1/(sqrt(2*pi*sd_1^2))
  if (is.null(d_2)) d_2 = 1/(sqrt(2*pi*sd_2^2))

  mu = (mu_1/sd_1^2 + mu_2/sd_2^2) / (1/sd_1^2 + 1/sd_2^2)
  sd = sqrt(1 / (1/sd_1^2 + 1/sd_2^2))
  d = dlike(mu, mu_1, sd_1, d_1,log=TRUE) + dlike(mu, mu_2, sd_2, d_2,log=TRUE)
  return(cbind(mu, sd, d))
}


# mu_1 =1
# sd_1 = 1
# mu_2 = -2
# sd_2 = 2
# d_1 = 3
# d_2 = 1
#
# new_gauss = combine_gaussians (mu_1, sd_1, mu_2, sd_2,log(3),log(1))
# new_gauss
# exp (new_gauss[3])
# abline (h= exp (new_gauss[3]))
#
# x = seq (-5,5,.001)
# y1 = 3/(1/sqrt(2*pi*sd_1^2))*dnorm(x, mu_1, sd_1)
# y2 = 1/(1/sqrt(2*pi*sd_2^2))*dnorm(x, mu_2, sd_2)
# y3 = y1*y2
#
# plot(x,y1, type='l', col='red')
# lines(x,y2, col='blue')
# lines(x,y3, col='green')
#
# x[which.max(y3)]
# max (y3)
# log(max (y3))















