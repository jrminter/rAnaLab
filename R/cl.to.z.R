#' Convert a confidence level to a Z-value
#'
#' Adapted from https://datascience.stackexchange.com/questions/10093/how-to-find-a-confidence-level-given-the-z-value
#'
#' @param cl A confidence level (real number)
#'
#' @return the Z-value
#'
#' @examples
#' library(rAnaLab)
#' print(cl.to.z(.95))
#'
#' @export
#'
cl.to.z <- function(cl){
  x <- 1.0-cl
  z <- qnorm(x/2.0, lower.tail=FALSE)
  return(z)
}
