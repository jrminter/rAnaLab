#' Construct a pallete from a PNG image
#'
#' Adapted from http://www.milanor.net/blog/build-color-palette-from-image-with-paletter/
#'
#' @param pngFile The png file to process
#'
#' @return a list (kmeans, rgb(k_means$centers))
#'
#' @import png
#' @import scales
#'
#' @examples
#' # Not run
#'
#' @export
#'
kmeans_pallete <- function(pngFile){
  # adapted from
  # http://www.milanor.net/blog/build-color-palette-from-image-with-paletter/
  img <- readPNG(pngFile)
  dimension <- dim(img)
  img_rgb <- data.frame( x = rep(1:dimension[2], each = dimension[1]),
                         y = rep(dimension[1]:1, dimension[2]),
                         R = as.vector(img[,,1]), #slicing our array into three
                         G = as.vector(img[,,2]),
                         B = as.vector(img[,,3]))
  k_means <- kmeans(img_rgb[,c("R","G","B")], centers = 5, iter.max = 30)
  show_col(rgb(k_means$centers))
  ret <- c(k_means, rgb(k_means$centers))
  ret
}


