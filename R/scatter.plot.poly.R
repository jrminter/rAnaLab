#' Scatter plot with optional polynomial regression
#'
#' A wrapper for level graphics functions to do a scatterplot
#' and perform an optional polynomial regression.
#'
#' @param x A vector of the independent variable
#' @param y A vector of the independent variable
#' @param delta A vector of the std. dev (deafult NULL)
#' @param bErrorBars A boolen to show error bars (default FALSE)
#' @param bReg A boolen to do the poly regression (default FALSE)
#' @param iReg Order of the regression (default 2)
#' @param str.x Title for x-axis (default "X")
#' @param str.y Title for y-axis (default "Y")
#' @param str.title Title for the plot (default "Plot")
#' @param v.cex Size for (default 1.2)
#' @param a.cex Size for title (default 1.2)
#' @param t.cex Size for tithe (default 1.2)
#' @param lw.fit Line width for fit (default 2)
#' @param lw.pts Line width for points (default 2)
#' @param lw.bars Line width for bars (default 2)
#' @param lw.ax Line width for axis (default 3)
#' @param len.bars Lenght of error bars (default 0.75)
#'
#' @examples
#' library(rAnaLab)
#' # set up simple vectors
#' ax <- c(1.00, 2.00, 3.00, 4.00, 5.00)
#' ay <- c(1.05, 2.05, 2.95, 4.05, 4.95)
#' ad <- c(0.05, 0.03, 0.04, 0.06, 0.05)
#' # do a plot and regression with error bars
#' the.fit <- scatter.plot.poly(ax, ay, ad, bReg=TRUE, bErrorBars=TRUE)
#'
#' @export
#'
#'
scatter.plot.poly <-
function(x,
         y,
         delta=NULL,
         bErrorBars=FALSE,
         bReg=FALSE,
         iReg=2,
         str.x = "X",
         str.y="Y",
         str.title="Plot",
         v.cex=1.2,
         a.cex=1.2,
         t.cex=1.2,
         lw.fit=2,
         lw.pts=2,
         lw.bars=2,
         lw.ax=3,
         len.bars =0.075)
{
   min.x <- min(x)
   max.x <- max(x)
   min.y <- min(y)
   max.y <- max(y)
   fit <- NULL
   if(bReg)
   {
      if(iReg==1)
      {
         fit <- lm(y ~ x)
      }
      else
      {
         fit <- lm(y ~ poly(x, iReg, raw=TRUE))
      }
      delta.x <- max.x - min.x
      x.fit <- seq(min.x - 0.1*delta.x, max.x + 0.1*delta.x, len = 200)
      y.fit <- predict(fit, data.frame(x=x.fit))
      min.x <- min(min.x,min(x.fit))
      max.x <- max(max.x,max(x.fit))
      min.y <- min(min.y,min(y.fit))
      max.y <- max(max.y,max(y.fit))
   }
   if(bErrorBars)
   {
      min.y <- min(y-1.96*delta)
      max.y <- max(y+1.96*delta)
   }
   x.temp <- c(min.x, max.x)
   y.temp <- c(min.y, max.y)
   x.t <- c(min.x, max.x)
   y.t <- c(min.y, max.y)
   plot(x.t, y.t, type="n", xlab="", ylab="", axes=FALSE) # setting up coord. system
   points(x, y, lwd=lw.pts, lty=3, pch = 1, col="blue")
   if(bReg)
   {
      lines(x.fit, y.fit,col="red", lwd=lw.fit )
   }
   if(bErrorBars)
   {
       # do the error bars
       arrows(x,y, x, y-1.96*delta, angle=90, code=2, length = len.bars, lwd = lw.bars)
       arrows(x,y, x, y+1.96*delta, angle=90, code=2, length = len.bars, lwd = lw.bars)
   }
   # draw an axis on the bottom
   axis(1,cex.axis=a.cex, lwd=lw.ax)
   # draw an axis on the left
   axis(2,cex.axis=a.cex, lwd=lw.ax)
   # draw a box around the plot
   box(lwd=lw.ax)
   mtext(str.x, side=1, line=2, cex=v.cex)
   mtext(str.y, side=2, line=2, cex=v.cex)
   mtext(str.title, side=3, line=1, cex=t.cex)

   fit

}

