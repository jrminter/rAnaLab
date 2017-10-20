#'Calculate lognormal bin breaks
#'
#'Use Roger Button's algorithm to compute the bin breaks for a lognormal
#'distribution with a reference bin value.
#'
#' @param vec vector of observations
#' @param n.root.2 The bin size (nth root of 2, default 8)
#' @param base.val number (default 0.1 ) for the reference bin break
#' @return A list of breaks
#'
#' @examples
#' library(rAnaLab)
#' # data
#' data(diam)
#' l.b <- calc.ln.bin.breaks(diam[,1], n.root.2=8, base.val=0.1)
#' print(l.b)
#'
#' @export
#'
#'

calc.ln.bin.breaks <- function(vec,
                               n.root.2=8.0,
                               base.val=0.1){

  min.val = min(vec)
  max.val = max(vec)

  hist.ratio <- 2.0^(1.0/n.root.2)

  while (min.val > base.val){
    base.val <- base.val * hist.ratio
  }
  while (min.val < base.val){
    base.val <- base.val / hist.ratio
  }
  # want to be sure to get the
  base.val <- base.val / hist.ratio

  num.bins <- round(1 + (log(max.val) - log(base.val)) / log(hist.ratio), 0)

  breaks <- rep(0, num.bins+1)
  for(n in seq(from=1, to=num.bins+1, by=1)){
    breaks[n] <- base.val * hist.ratio ^n
  }

  breaks
}
