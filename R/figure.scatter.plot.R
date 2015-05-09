#' Generate a scatterplot
#'
#' Generate a scatter plot for a figure.
#'
#' @param x A vector of independent values
#' @param y A vector of independent values
#' @param lab.x Label for X axis
#' @param lab.y Label for Y axis,
#' @param lab.cex Size (1.6)
#' @param ax.cex Size (1.6)
#' @param ax.lw  Line width (3)
#' @param pts.cex Points size (1.6)
#' @param pts.lw Points lw (2)
#' @param pts.pch Points style (1)
#' @param pts.col Points color ('blue')
#' @param new.mar Marhin array, e.g. c(4.7,4.7,1.1,1.8)
#' @param fixed.scale  (default = FALSE)
#' @param x.lim X limit (default=c(0,100))
#' @param y.lim Y limit (default=c(0,100))
#' @param ... Other graphics parameters
#'
#' @return  None
#'
#' @keywords keywords
#'
#' @export
#'
figure.scatter.plot <- function(x,
                                y,
                                lab.x='x',
                                lab.y='y',
                                lab.cex=1.6,
                                ax.cex=1.6,
                                ax.lw=3,
                                pts.cex=1.6,
                                pts.lw=2,
                                pts.pch=1,
                                pts.col='blue',
                                new.mar=c(4.7,4.7,1.1,1.8),
                                fixed.scale = FALSE,
                                x.lim=c(0,100),
                                y.lim=c(0,100),
                                ... )
  {
  old.mar <- par()$mar
  par(mar=new.mar)

  if (fixed.scale==TRUE){
    x.t <- x.lim
    y.t <- y.lim
    plot(x.t, y.t, type="n", xlab="", ylab="", axes=FALSE) # set up}
  } else {
    x.t <- c(min(x), max(x))
    y.t <- c(min(y), max(y))
    plot(x.t, y.t, type="n", xlab="", ylab="", axes=FALSE) # set up}

  }
  points(x, y, lwd=pts.lw, lty=3, pch=pts.pch, cex=pts.cex, col=pts.col)
  # draw an axis on the bottom
  axis(1,cex.axis=ax.cex, lwd=ax.lw)
  # draw an axis on the left
  axis(2,cex.axis=ax.cex, lwd=ax.lw)
  # draw a box around the plot
  box(lwd=ax.lw)
  mtext(lab.x, side=1, line=3, cex=lab.cex)
  mtext(lab.y, side=2, line=3, cex=lab.cex)
  par(mar=old.mar) #reset
}
