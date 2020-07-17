#' Compute the mass thickness in g/cm2
#'
#' @param rho - density (g/cm3)
#' @param t - thickness in cm
#'
#' @return mt - The mass thickness in g/cm2
#'
#'
#' library(rAnaLab)
#' a 20 nm layer of Mg3Al2Si3O2 (jeffbenite)
#' res <- mass.thickness(3.76, 20.0*10-07)
#' print(res)
#'
#'
#' @export
#'
mass.thickness <- function(rho, t){

  mt <- rho * t
  out <- sprintf("%.2f gm/cm^2", mt)
  print(out)
}

