#' Scatter plot with the mean and confidence interval.
#'
#' A wrapper for level graphics functions to do a scatter
#' plot and plot the mean and confidence interval.
#'
#' @param x vector of 'X' values
#' @param y vector of 'Y' values
#' @param delta vector of 'std dev' values (default NULL)
#' @param bErrorBars Boolean  (default FALSE) to show error bars
#' @param dAlpha confidence interval (default 0.95)  regression
#' @param str.x Title for x-axis (default "X")
#' @param str.y Title for y-axis (default "Y")
#' @param str.title Title for plot (default Plot")
#' @param v.cex Size for (default 1.2)
#' @param a.cex Size for axis (default 1.2)
#' @param t.cex Size for title (default 1.2)
#' @param lw.fit Line width for mean line (default 1.2)
#' @param lw.pts Line width for fit points (default 1.2)
#' @param lw.bars Line width for error bars (default 1.2)
#' @param lw.ax Line width for axes (default 3)
#' @param len.bars Width for error bars (default 0.75)
#'
#' @examples
#' library(rAnaLab)
#' # set up simple vectors
#' ax <- c(1.00, 2.00, 3.00, 4.00, 5.00)
#' ay <- c(1.05, 2.05, 2.95, 4.05, 4.95)
#' ad <- c(0.05, 0.03, 0.04, 0.06, 0.05)
#' # do a plot and regression with error bars
#' the.stats <- scatter.plot.stats(ax, ay, ad, bErrorBars=TRUE)
#'
#' @export
#'
#'

scatter.plot.stats <-
function(x,
         y,
         delta=NULL,
         bErrorBars=FALSE,
         dAlpha=0.95,
         str.x = "X",
         str.y="Y",
         str.title="Plot",
         v.cex=1.2,
         a.cex=1.2,
         t.cex=1.2,
         lw.fit = 2,
         lw.pts =2,
         lw.bars =2,
         lw.ax = 3,
         len.bars =0.075)
{
   min.x <- min(x)
   max.x <- max(x)
   min.y.ind <- min(y)
   max.y.ind <- max(y)
   y.mean <- mean(y)
   y.s    <- sd(y)
   z <- qnorm(1 - (1 - dAlpha)/2)
   max.y <-max(max.y.ind, y.mean+z*y.s)
   min.y <- min(min.y.ind, y.mean-z*y.s)
   if(bErrorBars)
   {
      min.y.ind <- min(y-z*delta)
      max.y.ind <- max(y+z*delta)
      max.y <-max(max.y.ind, y.mean+z*y.s)
      min.y <- min(min.y.ind, y.mean-z*y.s)
   }
   x.temp <- c(min.x, max.x)
   y.temp <- c(min.y, max.y)
   x.t <- c(min.x, max.x)
   y.t <- c(min.y, max.y)
   plot(x.t, y.t, type="n", xlab="", ylab="", axes=FALSE) # setting up coord. system
   points(x, y, lwd=lw.pts, lty=3, pch = 1, col="blue")

   if(bErrorBars)
   {
       # do the error bars - default 95 pct CI
       arrows(x,y, x, y-z*delta, angle=90, code=2, length = len.bars, lwd = lw.bars)
       arrows(x,y, x, y+z*delta, angle=90, code=2, length = len.bars, lwd = lw.bars)
   }
   # draw the mean and confidence limits (default 95%)
   abline(h=y.mean, lwd=lw.fit, col="red")
   abline(h=(y.mean+z*y.s), lwd=2, lty="dashed", col="red")
   abline(h=(y.mean-z*y.s), lwd=2, lty="dashed", col="red")
   # draw an axis on the bottom
   axis(1,cex.axis=a.cex, lwd=lw.ax)
   # draw an axis on the left
   axis(2,cex.axis=a.cex, lwd=lw.ax)
   # draw a box around the plot
   box(lwd=3)
   mtext(str.x, side=1, line=2, cex=v.cex)
   mtext(str.y, side=2, line=2, cex=v.cex)
   mtext(str.title, side=3, line=2.5, cex=t.cex)
   # str.head <- sprintf("mean=%.3f %.3f CI (%.3f, %.3f)",y.mean, dAlpha, y.mean-z*y.s, y.mean+z*y.s)
   # mtext(str.head, side=3, line= .25, cex=v.cex)

   # return mean, s, p, lci, uci
   v.stats = c(y.mean, y.s, dAlpha, y.mean-z*y.s, y.mean+z*y.s)

   v.stats
}

