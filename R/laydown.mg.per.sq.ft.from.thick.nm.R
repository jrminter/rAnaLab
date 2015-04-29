#' Compute the laydown in mg per sq ft
#'
#' Compute the laydown from thickness.
#'
#' @param t.nm  thickness in nm
#' @param density  in g/cm3
#' @return mg.per.sq.ft
#'
#' @export
laydown.mg.per.sq.ft.from.thick.nm <- function(t.nm, density){
  # Unit test from ICP 112.6 nm Ni -> 93.1 mg/sq ft
  # laydown.mg.per.sq.ft.from.thick.nm(112.6, 8.90))
  t.cm <- 1.0e-07 * t.nm
  g.sq.cm <- density * t.cm
  mg.sq.ft <- 9.29e5 * g.sq.cm
  mg.sq.ft
}
