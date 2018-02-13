#' Calculate normal and lognormal statistics for a vector
#'
#' Uses the EnvStats package under the hood
#'
#' @param vec A vector of observations to analyze
#'
#' @return A named list with statistics
#'
#' @import EnvStats
#'
#' @export
#'
#' @examples
#'
#' library(rAnaLab)
#' set.seed(42)
#' gmd <- 50.0
#' gsd <-  2.0
#' mu <- log(gmd)
#' sigma <- log(gsd)
#' vec <- rlnorm(10000, mu, sigma)
#' ret <- calc.normal.lognormal.stats(vec)
#' print(ret)

calc.normal.lognormal.stats <- function(vec){
  nobs <- length(vec)
  mu <- mean(vec, trim=0, na.rm=TRUE)
  s <- sd(vec, na.rm=TRUE)
  gmd <- geoMean(vec, na.rm=TRUE)
  gsd <- geoSD(vec, na.rm=TRUE, sqrt.unbiased=TRUE)

  ret <- c(nobs, mu, s, gmd, gsd)
  names(ret) <- c("nobs", "mu", "s", "geom mean", "geom sd")
  return(ret)
}
