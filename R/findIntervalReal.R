# NOTE: The non-deprecated version of this code is required by the new version
# of addMaturity(). However, some older code in our repos still uses the
# deprecated version.
#
# TODO: Check if and why the deprecated version is indeed required by older
# code. This may
# be related to NA's in the (cumulative) inputs of findIntervalReal(). Using
# recodeNAInCumulativeSeries() to preprocess the inputs may solve the issue.
# If necessary, fix it so that it works with the non-deprecated.
#' findIntervalReal
#'
#' as base::findInterval() but returns a real with 'index fraction'
#'
#' @examples \dontrun{
#' x <- 2:18
#' vec <- c(5, 4, 15) # edge case to test with
#' vec <- rep(NA, 11) # edge case to test with
#' vec <- c(5, 10, NA, 15) # edge case to test with
#' vec <- c(5, 10, NA, 15) # create two bins [5,10) and [10,15)
#' findInterval(x, vec)
#' findIntervalReal(x, vec)
#' cbind(x, findInterval(x, vec), findIntervalReal(x, vec))
#' cbind(x, findInterval(x, vec, rightmost.closed = TRUE), findIntervalReal(x, vec))
#' cbind(x, findInterval(x, vec, all.inside = TRUE), findIntervalReal(x, vec))
#' cbind(x, findInterval(x, vec, left.open = TRUE), findIntervalReal(x, vec))
#' }
#'
#' @export
findIntervalReal <- function(x, vec, useDeprecatedVersion = TRUE, ...) {
  if (NA %in% vec){
    vec <- aphApprox2(vec)
  }
  if (NA %in% vec){
    return(rep(NA, length(x)))
  }
  if (useDeprecatedVersion) {
    if (any(diff(vec) < 0)){
      log_warn("findIntercalReal| help")
      # vec <- c(3, 5, NA, -9, 11)
      vec[his.na(vec)] <- NA
      vec <- zoo::na.locf(vec, na.rm = FALSE)
      dvec <- diff0(vec)
      dvec[dvec < 0] <- 0
      vec <- cumsum(dvec) + vec[1]
    }
    ii <- findInterval(x, vec, rightmost.closed = TRUE)
    vecLow <- vec[pmax(1, ii)]
    vecHigh <- vec[ii + 1]
    frac <- (x - vecLow) / (vecHigh - vecLow)
    res <- ii + frac
    res[which(is.na(vecHigh))] <- length(vec)
    res[ii == 0] <- NA
    res[ii == length(vec)] <- NA
  } else {
    ii <- findInterval(x, vec, ...)
    vecLow <- vec[pmax(1, ii)]
    vecHigh <- vec[ii + 1]
    frac <- (x - vecLow) / (vecHigh - vecLow)
    res <- ii + frac
  }
  return(res)
}
