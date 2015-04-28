#' Compute the upper 95 percent confidence limit of a vector
#'
#' A helper function for several others
#'
#' @param x A vector of observations
#'
#' @return the upper 95 percent CL
#'
#' @export
#'
v.u.cl <- function(x){
  # 95 percent CI from t-distribution
  # from www.cyclismo.org/tutorial/R/confidence.html
  error <- v.std.err(x)*qt(0.975, df=length(x))
  ci.x.ll <- mean(x)+error
  ci.x.ll
}
