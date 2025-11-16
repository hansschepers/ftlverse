#' interNAZoo
#' 
#' does not replace NA on tails. uses zoo::na.approx
#' assumes equidistant data
#' 
#' @export
interNAZoo <- function(x, na.rm = FALSE, ...){
  as.numeric(
    zoo::na.approx(
      zoo::zoo(x), na.rm = na.rm, ...  # ... added 20201014
    ))
}


#' fillInternalNAs
#' @export
fillInternalNAs <- interNAZoo


# interNA2
# interNA2 <- interNAZoo


#' interNA0
#' 
#' replaces all NAs, uses stats::approx, with rule passed on
#' can take a separate x 
#' 
#' @param rule passed on to stats::approx
#' @examples \dontrun{
#'   y <- c(NA, NA, 5, 3, 1, NA, NA, NA, 2, 12, NA)
#'   interNA0(y)
#'   # implicitly it assumed equidistant x
#'   interNA0(y, x=11:21)
#'   interNA0(y, x=c(11:18, 10+19:21))
#'   interNAZoo(y)
#' }
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
interNA0 <- function(y, x=seq_along(along.with = y), rule = 2, ...) {
  nnonNAs <- length(x[!is.na(y)])
  if (nnonNAs <= 2){
    warning("interNA0| too few non-NAs")
    return(y)
  }
  approx(x=x, y=y, xout=x, rule=rule, ...)$y
}
