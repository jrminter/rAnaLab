#' Compute the thickness in nm from ug per sq cm
#'
#' Compute the thickness from laydown
#'
#' @param ug.sq.cm  laydown in ug.sq.cm
#' @param density  in g/cm3
#' @return t.nm
#'
#' @export
thick.nm.from.ug.per.sq.cm <- function(ug.sq.cm, density){
  # compute thickness in nm from laydown in ug/sq cm and
  # density in g/cm3
  g.sq.cm <- ug.sq.cm / 1.0e6
  t.cm <- g.sq.cm/density
  t.nm <- 1.0e7*t.cm
  t.nm
}
