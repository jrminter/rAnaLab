#'Simulate a bimodal lognormal distribution
#'
#'Simulates a bimodal lognormal distribution and bins the data
#'
#' @param fra1 The fraction of the first mode
#' @param gmd1 The geometric mean diameter of the first mode
#' @param gsd1 The geometric standard of the first mode
#' @param gmd2 The geometric mean diameter of the second mode
#' @param gsd2 The geometric standard of the second mode
#' @param num.sims The number of random draws (default 10000)
#' @param n.root.2 The bin size (nth root of 2, default 8)
#' @param base.val number (default 0.1 ) for the reference bin break
#' @return A dataframe ready to plot
#'
#' @examples
#'
#' library(rAnaLab)
#' binned <- sim.bimodal.lognormal(0.05, 6.0, 1.05, 30.0, 1.65,
#'                                 num.sims=1000, n.root.2=8.0,
#'                                 base.val=0.1)
#' print(head(binned))
#'
#' @export
#'
#'
sim.bimodal.lognormal <-
function(fra1, gmd1, gsd1, gmd2, gsd2, num.sims=10000,
         n.root.2=8.0, base.val=0.1)
{
  set.seed(42)
  fra2 <- 1.0-fra1
  mu1 <- log(gmd1)
  sigma1 <- log(gsd1)
  mu2 <- log(gmd2)
  sigma2 <- log(gsd2)

  n1 <- round(fra1*num.sims, 0)
  n2 <- round(fra2*num.sims, 0)
  vec1 <- rlnorm(n1, mu1, sigma1)
  vec2 <- rlnorm(n2, mu2, sigma2)

  vec <- c(vec1, vec2)
  binned <- rb.lognormal.bin(vec, n.root.2, base.val)
  diam <- binned[[1]]$x
  cts <- binned[[1]]$cts
  dens <- binned[[1]]$dens

  df <- data.frame(diam=diam, cts=cts, dens=dens)
  df
}
