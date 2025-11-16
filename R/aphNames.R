#' aphNames
#' 
#' @export
aphNames <- function(x){
  x <- gsub("[\u00B2]","2", x)
  x
}