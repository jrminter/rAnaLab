#' Compute the entry for a ADDA calibration string.
#'
#' Compute the mm per pixel for the maximum pixel size (4096) of a
#' Soft Imaging Systems ADDA-II slow scan interface. This is entered
#' into the channel calibration in the Analysis.ini file and is corrected
#' for subsampling (e.g. to 1024 pixels) for a given image within the
#' AnalySIS software
#'
#' @param px.per.unit - The number of pixels per unnit
#' @param x.px vector of 'Y' values
#' @param unit.fact vector of 'std dev' values (default NULL)
#'
#' @return px.per.mm
#'
#' @examples
#' library(rAnaLab)
#' x <- sprintf("H4100 800X: X= %.3f, %.3f",  calc.adda.mm.per.px(150.92),
#'      calc.adda.mm.per.px(148.77))
#' print(x)
#'

#'
#' @export
#'
#'
calc.adda.mm.per.px <- function(px.per.unit,
           x.px=1024.,
           unit.fact=1.0e-9)
{
   adda.max = 4096. # Max pix size for ADDA-II
   scale.fact = adda.max/x.px
   un.per.px = px.per.unit /scale.fact
   mm.per.px = un.per.px*1000.*unit.fact
   px.per.mm = 1.0/mm.per.px
   return(px.per.mm)
}
