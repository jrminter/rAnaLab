#' Parse a .csv file writteh by Gwyddion to a data frame
#'
#' Gwyddion writes a strange .csv file with semicolons as
#' delimiters and several header lines. This is a helper function
#' to parse the file into a dataframe
#'
#' @param fpath A path to the .csv file
#'
#' @param skip number of rows to skip
#'
#' @param vNames a vector of the row names for the dataframe
#'
#' @param mult a multiplier to convert quantities from meters. Default is 1.0e6 to convert to micrometers.
#'
#' @return the final dataframe
#'
#' @export
#'
gwyddion.csv.to.df <- function(fpath, skip, vNames, mult=1.0e6){
  df <- read.table(fpath, skip=skip, sep=';', as.is=TRUE)
  nc <- ncol(df)
  df <- df[,-nc] # get rid of the NA at the end of each line
  df <- mult*df
  names(df) <- vNames
  # print(head(df))
  df
}
