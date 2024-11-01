
#' @title Plot STM modelling results

#' Plot the posterior distribution of the formants and the fundamental frequency
#' given a template and a set of formant frequencies.
#'
#' @param df A dataframe of STM output
#' @param psi_range A vector of the range of psi values to plot
#' @param n_steps A scalar of the number of steps to take in the psi range
#' @examples
#' # TBD
#' @export
#' @importFrom graphics layout par plot lines points text rect abline
#' @importFrom graphics axis grid legend mtext
#' @importFrom stats dnorm dunif
#'

plot_posterior = function(df, psi_range = c(6.9, 7.6),
                          n_steps = 1000){

  cols = c("#F7B5C5BF", "#27C0D8BF", "#F8A61BBF", "#0C8275BF", "#DB686FDC",
             "#822B32BF", "#3A65AFBF", "#FF6400BF", "#413069B4", "#94CB94C8",
             "#FEEF94C8", "#88021BB4", "#719F9FDC", "#CA87B9BF", "#6C0F2DC8")

  if (inherits(df, "STM_output")) df = df$df

  old_par = par(no.readonly = TRUE)

  par (mfrow = c(3,1), mar = c(.2,2.2,.1,.1), oma = c(4,.5,.5,.5))

  if (is.na(df$prior_mu[1])){
    psi_prior = cbind(seq(psi_range[1], psi_range[2], length.out = n_steps),
                      dunif(seq(psi_range[1], psi_range[2], length.out = n_steps),
                                psi_range[1], psi_range[2]))
    df$prior_density[1] = log(psi_prior[1,2])
  } else{
    psi_prior = dlike (NULL, df$prior_mu[1],df$prior_sd[1],df$prior_density[1])
  }

  plot (psi_prior[,1], psi_prior[,2], ylim = c(0,exp(df$prior_density[1]))*c(1,1.1),
        type = 'l', col=cols[5],lwd=4, xaxt='n', xlab='',xaxs='i',yaxs='i',
        ylab="", xlim = psi_range,yaxt='n')
  grid()

  plot (0, type = 'n',ylim=c(0,max(exp(df$likelihood_density)))*c(1,1.1),
        xaxs='i', xaxt='n', xlab='',yaxs='i',ylab="",cex.lab=1.5,
        xlim = psi_range,yaxt='n')
  grid()
  for (i in 1:nrow(df)){
    v_likelihoods = dlike (NULL, df$likelihood_mu[i],df$likelihood_sd[i],df$likelihood_density[i])
    lines (v_likelihoods[,1], v_likelihoods[,2], col = cols[i],lwd=4)
  }

  legend ("topleft", legend = rownames(df), col = cols,
          lwd = 4,bty='n',cex=1.2, ncol=2)
  mtext (bquote("Density"), side=2,line=1)

  plot (0, type = 'n',ylim=c(0,max(exp(df$posterior_density)))*c(1,1.1),
        xaxs='i', xlab='',yaxs='i',ylab="",cex.lab=1.5,yaxt='n',
        xlim = psi_range)
  grid()
  for (i in 1:nrow(df)){
    v_posteriors = dlike (NULL, df$posterior_mu[i],df$posterior_sd[i],df$posterior_density[i])
    lines (v_posteriors[,1], v_posteriors[,2], col = cols[i],lwd=4)
  }
  mtext (bquote("Psi"), side=1,line=3)

  par(old_par)

}













