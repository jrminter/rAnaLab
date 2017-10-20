#'Bin a distribution for analysis as a sum of lognormal modes
#'
#'Uses Roger Button's algorithm to computes equally-spaced logarithmic
#'bins from a vector of observations and computes the parameters for a
#'single mode lognormal distribution.
#'
#' @param x A vector of observations
#' @param n.root.2 The bin size (nth root of 2, default 8)
#' @param base.val number (default 0.1 ) for the reference bin break
#' @param do.plot Boolean (default FALSE) to plot histogram
#' @return A list with a data frame, The mean of log(x), and the stad. dev. of log(x)
#'
#' @examples
#' library(rAnaLab)
#' # data
#' data(diam)
#' l.b <- lognormal.bin(diam[,1])
#' print(str(l.b))
#'
#'
#' @export
#'
#'
rb.lognormal.bin <-
  function(x,
           n.root.2=8.0,
           base.val=0.1,
           do.plot=FALSE)
  {
    x <- subset(x, x>0)
    min.x <- min(x)
    max.x <- max(x)
    l.mu  <- mean(log(x))
    l.sd  <- sd(log(x))
    l.min.x <- log(min.x)
    l.max.x <- log(max.x)

    bins <- calc.ln.bin.breaks(x, n.root.2, base.val)

    h <- hist(x, breaks=bins, plot=do.plot)
    # save what we want
    # midpoints
    h.l.x <- log(h$mids)
    h.x <- h$mids
    # counts
    h.cts <- h$counts
    # densities
    h.dens <-h$density
    # make a data frame
    data <- data.frame(log.x=h.l.x,
                       x=h.x,
                       cts=h.cts,
                       dens=h.dens)
    # return a list
    out <-list(data, l.mu, l.sd)
    out
  }
