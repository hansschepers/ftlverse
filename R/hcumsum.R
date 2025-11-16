#' aphCumsum
#' @examples \dontrun{
#'   x <- c(NA, NA, 8, NA, 6, 9, NA, NA, NA)
#'   NATails(x)+0
#'   seqMiddle(x)
#'   # indexOfFirstDatapoint:
#'   which(!NATails(x))[1]
#'   firstNonNA(x)
#'   aphCumsum(x)
#'   aphCumsum(x, start0 = 1)
#'   aphCumsum(x, interpolate = TRUE)
#'   cumsum(x)
#'   x
#'   aphCumsum(x, interpolate = TRUE, fillLeftNA = TRUE, reNAtails = FALSE)
#' }
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
aphCumsum <- function(x
                    , na.rm = TRUE
                    , start0 = 0    # changed! 20201020
                    , reNAmiddle = FALSE
                    , reNAtails = TRUE # was FALSE until 20220824   # TRUE 20220804??
                    , fillLeftNA = FALSE
                    , interpolate = FALSE) {
  nnonNAs <- sum(!is.na(x))
  if (nnonNAs < 1){
    warning("aphLite: aphCumsum| few nnonNAs")
    return(x)
  }

  wereTails <- NATails(x)
  middleNAs <- is.na(x) & !NATails(x)
  x2 <- x
  if (interpolate) {
    x2 <- fillInternalNAs(x2, na.rm = FALSE)
  } else {
    x2[middleNAs] <- 0
  }
  x2
  indexOfFirstDatapoint <- which(!NATails(x2))[1]
  if (is.na(indexOfFirstDatapoint)) indexOfFirstDatapoint <- 1
  # print(indexOfFirstDatapoint)
  x2[wereTails] <- 0
  x2 <- cumsum(x2)
  if (start0 > 0) {
    x2 <- x2 - pmax(0, pmin(1, start0)) * x[indexOfFirstDatapoint]
  # } else {
  #   fillLeftNA <- TRUE
  }
  if (reNAmiddle) {
    x2[middleNAs] <- NA
  }
  if (reNAtails) {
    x2[wereTails] <- NA
  }
  if (fillLeftNA){
    x2[1:(indexOfFirstDatapoint-1)] <- 0 # x2[indexOfFirstDatapoint]
  }
  x2
}



#' hcumsum
#' @export
hcumsum <- aphCumsum
