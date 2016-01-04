#' Get the modification date for a file
#'
#' Parses the file.info for a file to get and return the modification date
#'
#' @param inPath A file path to be corrected
#'
#' @return the corrected path
#'
#' @export
#'

get.file.mod.date <- function(inPath){
  modDateTime <- as.character(file.info(inPath)$mtime)
  lModDate <-  strsplit(modDateTime, " ")
  strModDate = lModDate[[1]][1]
  strModDate
}
