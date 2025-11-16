#' Detect NA values at the beginning or end of a vector
#'
#' Returns a vector with boolean values for NA's but only if they are at the
#' beginning or the end.
#'
#' @examples \dontrun{
#'   x <- c(NA, NA, 5, 3, 1, NA, NA, NA, 2, 12, NA)
#'   NATailsPR(x)
#'   NATailsHS(x)
#'   seqMiddleHS(x)
#'   seqMiddle(x)
#'   seqMiddle(x)
#'   findTails(x)
#' }
#' @seealso findTails
#' @export
NATailsPR <- function(x){
  is.na(zoo::na.approx(zoo::zoo(x), na.rm = FALSE))
}

#' NATailsHS
#' 
#' @export
NATailsHS <- function(x){
  is.na(interNAZoo(x))
}


#' seqMiddleHS
#' 
#' @export
seqMiddleHS <- function(x){
  x2 <- cumsum(!NATailsHS(x))
  x2[NATailsHS(x)] <- NA
  x2
}
