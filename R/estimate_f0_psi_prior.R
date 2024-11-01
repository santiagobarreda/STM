
#' Estimate the prior distribution of f0 given psi
#'
#' This function estimates the joint prior distribution of f0 and psi.
#' It also returns the peak log density of the prior distribution.
#'
#' @param f0 A scalar of fundamental frequency
#' @param PSI_prior_mean A scalar of the mean of the prior distribution of psi
#' given f0
#' @param PSI_prior_sd A scalar of the standard deviation of the prior distribution of psi
#' given f0
#' @param f0_hat_sd A scalar of the standard deviation of the estimated f0 given psi
#' @param f0_hat_intercept A scalar of the intercept of the estimated f0 given psi
#' @param f0_hat_slope A scalar of the slope of the estimated f0 given psi
#' @param ... additional parameters
#' @return A list containing the mean and standard deviation of the prior distribution
#' of f0 given psi, and the log density at that location.
#' @examples
#' # TBD
#' @export

estimate_f0_psi_prior =
  function(f0, PSI_prior_mean=7.233, PSI_prior_sd = 0.1284,f0_hat_sd=0.1327,
           f0_hat_intercept=-10.32, f0_hat_slope=2.145, ...){

    intercept_f0_given_psi = ((f0_hat_slope*f0)/(f0_hat_sd^2) -
                                (f0_hat_intercept*f0_hat_slope)/(f0_hat_sd^2))
    slope_f0_given_psi = ((-f0_hat_slope^2 / (f0_hat_sd^2)))
    f0_given_psi_mu = (-intercept_f0_given_psi/slope_f0_given_psi)
    f0_given_psi_sd = sqrt(-1/slope_f0_given_psi)
    f0_hat = f0_hat_intercept +f0_hat_slope*f0_given_psi_mu

    f0_given_psi_density = stats::dnorm (f0, f0_hat, f0_hat_sd, log=TRUE)
    psi_prior_density = stats::dnorm (PSI_prior_mean, PSI_prior_mean, PSI_prior_sd, log=TRUE)

    prior_density = f0_given_psi_density + psi_prior_density
    tmp = combine_gaussians(f0_given_psi_mu, f0_given_psi_sd,
                            PSI_prior_mean, PSI_prior_sd,
                            f0_given_psi_density, psi_prior_density)

    output = c(prior_mu=tmp[1], prior_sd=tmp[2], prior_density=tmp[3])
    return (output)
}


#
# psis = seq(6.8,7.6,.001)
# f0_hat = f0_hat_intercept +f0_hat_slope*psis
# densities = dnorm (f0, f0_hat, f0_hat_sd, log=FALSE)
# densities2 = dnorm (psis, PSI_prior_mean, PSI_prior_sd, log=FALSE)
# densities3 = densities * densities2
# max(densities3)
#
# plot (psis, densities, type = 'l', ylim = c(0,4))
# lines (psis, densities2, type = 'l', col=2)
# lines (psis, densities3, type = 'l', col=3)
#
#
# densities_ = dnorm (psis, f0_given_psi_mu, f0_given_psi_sd)
# densities_ = exp(f0_given_psi_density) * densities_ * sqrt(2*pi*f0_given_psi_sd^2)
#
# lines (psis, densities_, col = 2, lwd=2)
#
# qq = combine_gaussians (PSI_prior_mean,PSI_prior_sd,
#                    f0_given_psi_mu, f0_given_psi_sd,
#                    psi_prior_density, f0_given_psi_density)
# exp(qq[3])


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

