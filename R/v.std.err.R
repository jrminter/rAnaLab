#' Compute the standard error of a vector
#'
#' A helper function for several others
#'
#' @param x A vector of observations
#'
#' @return the standar error
#'
#' @export
v.std.err <- function(x){
  ret <- sd(x)/sqrt(length(x))
  ret
}
