#' Remove backslashes from a file path
#'
#' Replaces backslashes in a file path with forward slashes
#'
#' @param inPath A file path to be corrected
#'
#' @return the corrected path
#'
#' @export
#'
fix.path <- function(inPath){
  dos <- "\\\\"
  unix <- "/"
  outPath <- gsub(dos, unix, inPath)
  return(outPath)
}
