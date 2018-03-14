#' Convert a Z-value to a confidence level
#'
#' Adapted from https://datascience.stackexchange.com/questions/10093/how-to-find-a-confidence-level-given-the-z-value
#'
#' @param z A Z-value (real number)
#'
#' @return the confidence level
#'
#' @examples
#' library(rAnaLab)
#' print(z.to.cl(.950))
#'
#' @export
#'
z.to.cl <- function(z){
  val <- pnorm(z, lower.tail=FALSE)
  cl <- 1.0 - (2.0*val)
  return(cl)
}
