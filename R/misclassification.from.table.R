#' Misclassification index from a table
#'
#' Compute the misclassification index from a table.
#' From: Patrick Breheny
#' http://r.789695.n4.nabble.com/misclassification-rate-td3787075.html
#'
#' @param x a table
#'
#' @return the misclassification index
#'
#' @export
misclassification.from.table <- function(x){
  # from Patrick Breheny
  # http://r.789695.n4.nabble.com/misclassification-rate-td3787075.html
  misclass <- 1-sum(diag(x))/sum(x)
  misclass
}
