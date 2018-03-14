#' Compute the number of particles for image analysis to get a desired precision
#'
#' Implement in R the computation for for the number of grains required for
#' a desired precision measurement by electron images analysis of a lognormal
#' distribution with a given geometric standard deviation assuming a considence
#' level of the result ad a specified relative error. This was ported by John
#' Minterfrom an Excel computation by Roger Button.
#'
#' @param gsd (float) The geometric standard deviation for the lognormal distribution
#'
#' @param c_level (float, default 0.95 ) The confidence level desired for the measurement
#'
#' @param rel_error (float, default 0.01) The maximum acceptible relative error for the measurement
#'
#' @examples
#'
#' a <- particles.to.measure(2.0, 0.95, 0.01)
#' print(a)
#'
#' @export


particles.to.measure <- function(gsd, c_level=0.95, rel_error=0.01){
	omega <- qnorm(c_level)^2*log(gsd)^2
	ng <- exp(-2.0*log(rel_error)+log(omega))
	return(ng)
}
