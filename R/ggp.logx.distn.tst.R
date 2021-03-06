#' Make a test plot of the diameter distribution using ggplot
#'
#' use this to get parameters for the axes. Then use ggp.logx.distn
#'
#' @param binned  A dataframe generated by make.log.bin.df
#' @param m.title A short title for the plot
#' @param x.units Integer: -3 (mm), -6 (um), -9 (nm), Default = -6.
#' @param d.max The maximum density for the plot. Default = -1 (calculate)
#'
#' @import ggplot2
#'
#' @examples
#'
#' library(rAnaLab)
#' data(diam)
#' l.b <- rb.lognormal.bin(diam[,1], n.root.2=8)
#' print(head(l.b))
#' binned <- make.log.bin.df(l.b)
#' print(head(binned))
#'
#' plt <- ggp.logx.distn.tst(binned,
#'                       "AgX Grain Diameter",
#'                       x.units=-9,
#'                       d.max=0.025)
#'
#' print(plt)
#'
#' @export
#'
ggp.logx.distn.tst <- function(binned, m.title, x.units=-6, d.max=-1)
{

  md <- max(binned$dens)
  nP <- sum(binned$cts)

  if (d.max < 0){
    d.max <- 1.1*md
  }

  if(x.units == -3) {
    x.lab = "ECD [mm]"
  } else if(x.units == -6){
    x.lab = "ECD \U00B5m"
  } else {
    x.lab = "ECD [nm]"
  }

  plotECD <- ggplot() +
    geom_point(data=binned, aes(x=diam, y=dens),
               colour="darkblue") +
    scale_x_log10() +
    ylim(0, d.max) +
    xlab(x.lab) +
    ylab("Density") +
    ggtitle(sprintf("%s (n=%d)", m.title, nP)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12),
          plot.title=element_text(hjust = 0.5)) # center the title
  plotECD
}
