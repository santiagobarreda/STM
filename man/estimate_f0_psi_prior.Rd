% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_f0_psi_prior.R
\name{estimate_f0_psi_prior}
\alias{estimate_f0_psi_prior}
\title{Estimate the prior distribution of f0 given psi}
\usage{
estimate_f0_psi_prior(
  f0,
  PSI_prior_mean = 7.233,
  PSI_prior_sd = 0.1284,
  f0_hat_sd = 0.1327,
  f0_hat_intercept = -10.32,
  f0_hat_slope = 2.145,
  ...
)
}
\arguments{
\item{f0}{A scalar of fundamental frequency}

\item{PSI_prior_mean}{A scalar of the mean of the prior distribution of psi
given f0}

\item{PSI_prior_sd}{A scalar of the standard deviation of the prior distribution of psi
given f0}

\item{f0_hat_sd}{A scalar of the standard deviation of the estimated f0 given psi}

\item{f0_hat_intercept}{A scalar of the intercept of the estimated f0 given psi}

\item{f0_hat_slope}{A scalar of the slope of the estimated f0 given psi}

\item{...}{additional parameters}
}
\value{
A list containing the mean and standard deviation of the prior distribution
of f0 given psi, and the log density at that location.
}
\description{
This function estimates the joint prior distribution of f0 and psi.
It also returns the peak log density of the prior distribution.
}
\examples{
# TBD
}
