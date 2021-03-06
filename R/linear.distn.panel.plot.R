#' A three panel plot with linear binning
#'
#' Computes a histogram with linear binning, plotting the
#' mean and kernel density as well. Does a boxplot and a
#' qqplot, all in a single panel.
#'
#' @param v.dat A data vector
#' @param n.brks Number of breaks for the histogram  (default 500)
#' @param distn.lab Label for distribution (default "")
#' @param hist.legend Show histogram legend (default TRUE)
#' @param legend.loc Legend location (default 'topright')
#' @param kern.bw kernel for smoothing (default "nrd0")
#' @param plt.median Plot the median (default TRUE)
#' @param scale.mult Scale mutliplier for legend (default 1.2)
#' @param ... Other graphical parameters
#'
#' @export
#'
linear.distn.panel.plot <-
function(v.dat, n.brks=50, distn.lab="",
         hist.legend=TRUE, legend.loc='topright',
         kern.bw = "nrd0",
         plt.median=TRUE, scale.mult=1.2, ...){
  par(mfrow=c(1,3))
  # firsthistogram with empirical kernel
  # density plot
  n.pts <- length(v.dat)
  v.med <- median(v.dat)
  v.mu  <- mean(v.dat)
  v.sd  <- sd(v.dat)
  h <- hist(v.dat, breaks=n.brks, plot=FALSE)
  d.max <- max(h$density)
  hist(v.dat, breaks=n.brks, main=NULL, probability=TRUE,
       ylim=c(0, scale.mult*d.max), xlab=distn.lab)
  if(plt.median){
    x.t <- c(v.med, v.med)
    y.t <- c(0, d.max)
    lines(x.t, y.t, col='blue', lw=2)
  }
  lines(density(v.dat, kern.bw), col='red')
  if(hist.legend){
    if(plt.median){
      legend(x=legend.loc,
             c('data', 'kernel', 'median'),
               lty=c(1,1,1),col=c('black','red', 'blue'))
    } else {
      legend(x=legend.loc, c('data','kernel density'),
            lty=c(1,1),col=c('black', 'red'))
    }
  }
  boxplot(v.dat, outchar=T, main=NULL, ylab=distn.lab,
          col="gray80", ...)
  if(nchar(distn.lab) >0){
    qqnorm(v.dat, col='black',
           xlab='Theoretical Quantiles',
           ylab=paste0('Sample Quantiles ', distn.lab), main=NULL, ...)
  } else{
    qqnorm(v.dat, col='black',
           xlab='Theoretical Quantiles',
           ylab='Sample Quantiles', main=NULL, ...)
  }

  qqline(v.dat, col='red', lw=2)
  par(mfrow=c(1,1))
}
