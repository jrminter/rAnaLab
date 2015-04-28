#' Normal distribution binning
#'
#' Computes equally-spaced bins from a vector and computes the parameters for a single mode Gaussian distribution.
#'
#' @param x A vector of observations
#' @param do.plot Boolean (default FALSE) to plot histogram
#' @param nBreaks The number of bins (default 10)
#'
#' @return list with a data frame, The mean of x, and the std. dev. of x
#'
#' @examples
#'
#' require(rAnaLab)
#' # data
#' data("diam", package = "rAnaLab")
#' l.b <- normal.bin(diam[,1])
#'
#' @export
normal.bin <- function(x, do.plot=FALSE, nBreaks=10)
{
  # now compute the histogram
  h <- hist(x, breaks=nBreaks, plot=do.plot)
  # save what we want
  # midpoints
  h.x <- h$mids
  # counts
  h.cts <- h$counts
  # densities
  h.dens <-h$density
  # make a data frame
  data <- data.frame( x=h.x,
                      cts=h.cts,
                      dens=h.dens)
  # return a list
  mu <- mean(x)
  s <- sd(x)
  out <-list(data, mu, s)
  out
}
