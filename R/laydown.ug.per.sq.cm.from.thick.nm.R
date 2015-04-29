#' Compute the laydown in ug per sq cm
#'
#' Compute the laydown from thickness.
#'
#' @param t.nm  thickness in nm
#' @param density  in g/cm3
#' @return ug.sq.cm
#'
#' @export
laydown.ug.per.sq.cm.from.thick.nm <- function(t.nm, density){
  # Unit test from Pouchou 2002a for Zn 36 nm -> 26 ug/sq.cm
  # print(laydown.ug.per.sq.cm.from.thick.nm(36, 7.14))
  t.cm <- 1.0e-07 * t.nm
  g.sq.cm <- density * t.cm
  ug.sq.cm <- 1.0e6 * g.sq.cm
  ug.sq.cm
}
