#' Make a dataframe from lognoramlly binned data ready for ggplot
#'
#' A helper function to prevent repeatedly writing the same code...
#'
#' @param l.b.out The output structure from lognormal.bin
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
#' @export
#'
make.log.bin.df <- function(l.b.out){
  diam <- l.b.out[[1]]$x
  cts  <- l.b.out[[1]]$cts
  dens <- l.b.out[[1]]$dens

  binned <- data.frame(diam=diam, cts=cts, dens=dens)
  binned
}
