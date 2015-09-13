#' Compute a vector of absolute deviations from a vector
#'
#' A helper function for several others
#'
#' @param x A vector of observations
#'
#' @return a vector of absolute deviationsL
#'
#' @export
#'
v.abs.dev <- function(x){
  n <- length(x)
  y <- c(0,as.vector(sqrt((x[2:n] - x[1:(n-1)])**2)))
  y
}
