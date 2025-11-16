#' hcumsumOld
#' @examples
#' \dontrun{
#' x <- rep(NA, 8)
#' x <- 1:6
#' x <- c(NA, NA, 8, 8, 9, NA, NA, NA)
#' NATails(x) + 0
#' seqMiddle(x)
#' x <- c(NA, 3, 1, NA, NA, 8, 8, 9, NA, NA, NA)
#' x
#' NATails(x) + 0
#' seqMiddle(x)
#' # indexOfFirstDatapoint:
#' which(!NATails(x))[1]
#' hcumsumOld(x)
#' hcumsumOld(x, start0 = 0)
#' aphCumsumOld(x)
#' hcumsumOld(x, interpolate = TRUE)
#' aphCumsumOld(x, interpolate = TRUE)
#' aphCumsumOld(x, interpolate = TRUE, reNA = TRUE)
#' x <- c(3, x)
#' x
#' NATails(x) + 0
#' seqMiddle(x)
#' hcumsumOld(x)
#' hcumsumOld(x, start0 = 0)
#' cumsum(x)
#' hcumsumOld(x, start0 = 0)
#' }
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hcumsumOld <- function(x,
                    na.rm = TRUE,
                    start0 = 1,
                    reNA = FALSE # TODO
                    , interpolate = FALSE) {
  nnonNAs <- sum(!is.na(x))
  if (nnonNAs <= 1) {
    warning("hcumsumOld| few nnonNAs")
    return(x)
  }
  
  wereTails <- NATails(x)
  middleNAs <- is.na(x) & !NATails(x)
  x2 <- x
  if (interpolate) {
    x2 <- interNAZoo(x2, na.rm = FALSE)
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
    x2 <- x2 - start0 * x2[indexOfFirstDatapoint]
  }
  x2[1:indexOfFirstDatapoint] <- x2[indexOfFirstDatapoint]
  if (reNA) x2[middleNAs] <- NA
  if (reNA) x2[wereTails] <- NA
  
  x2
}


#' aphCumsumOld
#' @export
aphCumsumOld <- function(x,
                      na.rm = TRUE,
                      start0 = 0,
                      reNA = FALSE,
                      interpolate = FALSE) {
  hcumsumOld(
    x = x,
    na.rm = na.rm,
    start0 = start0,
    reNA = reNA,
    interpolate = interpolate
  )
}
