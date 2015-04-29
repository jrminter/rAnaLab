#' Compute the thickness in nm from mg per sq ft
#'
#' Compute the thickness from laydown
#'
#' @param mg.sq.ft laydown n mg.sq.ft
#' @param density  in g/cm3
#' @return t.nm
#'
#' @export
thick.nm.from.mg.per.sq.ft <- function(mg.sq.ft, density){
  # compute thickness in nm from laydown in mg/sq ft and
  # density in g/cm3
  g.sq.cm <- mg.sq.ft / 9.29e5
  t.cm <- g.sq.cm/density
  t.nm <- 1.0e7*t.cm
  t.nm
}
