#' Create a three panel plot for a column of observationd from a dataframe
#'
#' Plots a histogram, a box-plot, and a qq-plot using ggplot2
#'
#' @param df A dataframe containing the desired column.
#'
#' @param col.num The number of the column to use
#'
#' @param nam (default 'ECD') - label for the measured quantity
#'
#' @param un (default 'nm') - the unit string
#'
#' @param lw (default 1.25) - width of drawn lines
#'
#' @param  bins (default 30) - number of histogram bins
#'
#' @param ti A title for the top (default '')
#'
#' @return none - just does the grid plot
#'
#' @export

gg.panel.plot <- function(df, col.num, nam='ECD', un='nm',
                          lw=1.25, bins=30, ti=''){
  lightblue <- "#3A89C9"
  valLabel <- paste0(nam, " [", un, "]")
  vec <- df[, col.num]
  mu <- median(vec)
  df <- data.frame(nam=vec)
  plot1 <- ggplot(data=df, aes(nam)) +
    geom_histogram(bins=bins, aes(y = ..density..), fill=lightblue) +
    xlab(valLabel) +
    stat_function(fun=dnorm, args=list(mean=mean(vec), sd=sd(vec)),
                  lwd=lw, col='red') +
    geom_vline(xintercept=mu, size=lw)

  plot2 <- ggplot(data=df, aes(x="", y=nam)) +
    geom_boxplot(fill=lightblue) +
    xlab("") + ylab(valLabel)

  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  plot3 <- ggplot(data=df, aes(sample=nam)) +stat_qq() +
    xlab("normal quantiles") + ylab(valLabel) +
    geom_abline(slope=slope, intercept=int, colour='red', size=lw)

  if (nchar(ti>0)) {
    grid.arrange(plot1, plot2, plot3, ncol=3, top = ti)
  } else {
    grid.arrange(plot1, plot2, plot3, ncol=3)
  }
}
