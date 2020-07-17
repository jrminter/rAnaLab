#' Convert the mass thickness in ug/cm2 to g/cm2
#'
#'
#' @param rho - Input density (g/cm3)
#' @param rho.t.ug.cm2  The mass thickness in ug/cm2
#'
#' @return rho.t.g.cm2 The mass thickness in g/cm2
#'
#' @examples
#' library(rAnaLab)
#' rho.t.g.cm2 <- calc.rho.t.g.cm2(rho, rho.t.ug.cm2)
#' print(rho.t.g.cm2)
#'
#'
#' @export
#'
#'
calc.rho.t.g.cm2 <- function(rho, rho.t.ug.cm2){

  # there are 10^6 g in a uG
  rho.t.g.cm2 <- 1.0e-6 * rho.t.ug.cm2
  return(rho.t.g.cm2)
}
