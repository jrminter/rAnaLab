#' Print Eagleson's Law
#'
#' Print out Eagleson's Law. Emulated cowsay.
#'
#' @return a multiline string
#'
#' @export
#'
#' @examples
#' library(rAnaLab)
#' eaglesons.law()
#'
eaglesons.law <- function(){
  cat("Eagleson's Law:\n\n")
  cat("Any code of your own that you\n")
  cat("haven't looked at for 6+ months\n")
  cat("might as well have been written\n")
  cat("by someone else.\n\n")
  cat("Eagleson is an optimist. The real\n")
  cat("number is closer to 3 weeks!\n")
  cat("    ----------------      \n")
  cat("    \\   ^__^              \n")
  cat("     \\  (oo)\\_______      \n")
  cat("        (__)\\       )\\/\\  \n")
  cat("            ||----w |     \n")
  cat("            ||     ||     \n")
}
