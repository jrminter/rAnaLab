#'Simulate a lognormal distribution
#'
#'Simulates lognormal distribution and bins the data
#'
#' @param gmd The geometric mean diameter of the first mode
#' @param gsd The geometric standard of the first mode
#' @param num.sims The number of random draws (default 10000)
#' @param n.root.2 The bin size (nth root of 2, default 8)
#' @param base.val number (default 0.1 ) for the reference bin break
#' @return A dataframe ready to plot
#'
#' @examples
#'
#' library(rAnaLab)
#' binned <- sim.lognormal(30.0, 1.65, num.sims=1000,
#' n.root.2=8.0, base.val=0.1)
#' print(head(binned))
#'
#' @export
#'
#'
sim.lognormal <-
function(gmd, gsd, num.sims=10000, n.root.2=8.0, base.val=0.1)
{
  set.seed(42)
  mu <- log(gmd)
  sigma <- log(gsd)

  vec <- rlnorm(num.sims, mu, sigma)

  binned <- rb.lognormal.bin(vec, n.root.2, base.val)
  diam <- binned[[1]]$x
  cts <- binned[[1]]$cts
  dens <- binned[[1]]$dens

  df <- data.frame(diam=diam, cts=cts, dens=dens)
  df
}
