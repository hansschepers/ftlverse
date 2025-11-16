#' aphApprox2
#' 
#' @examples \dontrun{
#'   aphApprox2(9)
#'   aphApprox2(c(NA, NA, 9, NA))
#'   aphApprox2(c(NA, NA, 9, Inf, NA))
#'   aphApprox2(c(NA, NA, 9, Inf, NA), InfAsNA = TRUE)
#'   aphApprox2(c(NA, 8, NA, 9, Inf, NA), InfAsNA = TRUE)
#'   aphApprox2(c(NA, 8, NA, 9, Inf, NA), InfAsNA = TRUE, rule = 1)
#'   aphApprox2(c(NA, Inf))
#'   aphApprox2(c(NA, NA))
#' }
#' @importFrom logger log_threshold
#' @export
aphApprox2 <- function(y
                       , x = seq_along(along.with = y)
                       , rule = 2
                       , InfAsNA = FALSE
                       , extendTo = NULL
                       , ...) {
  if (length(y) == 0) {
    return(y)
  }
  if (!is.null(extendTo)){
    if (extendTo > length(y)){
      y <- c(y, rep(NA, extendTo - length(y)))
    }
  }
  if (InfAsNA){
    y[his.na(y)] <- NA
  }
  nnonNAs <- sum(!is.na(y))
  if (nnonNAs < 1) {
    logger::log_debug("aphApprox2| no non-NA data, returning original")
    if (log_threshold() > 550){
      print(match.call())
    }
    return(y)
  }
  if (nnonNAs == 1) {
    logger::log_debug("aphApprox2| just one non-NA value, returning that")
    if (log_threshold() > 550){
      print(match.call())
    }
    return(rep(hmean(y), length(y)))
  }
  stats::approx(x = x, y = y, xout = x, rule = rule, ...)$y
}


#' Interpolate Missing Data Linearly
#'
#' Given a vector of data, interpolate any missing values.
#'
#' Note that if only 1 non-NA value is detected, it will be returned, repeated for the length of the original input.
#'
#' @param y `numeric(n)`. The vector to be interpolated.
#'
#' @return A `numeric(n)` vector.
#'
#' @importFrom stats approx
interpolateLinearly <- function(y) {
  notMissing <- !is.na(y)
  sumNotMissing <- sum(notMissing)
  if (sumNotMissing < 1L) {
    logger::log_debug("All data are missing so there is nothing to approximate from; returning the original input.")
    return(y)
  }
  if (sumNotMissing == 1L) {
    logger::log_debug("There is only one non-NA value, returning a vector with only this value.")
    return(rep(y[notMissing], length(y)))
  }
  x <- seq_along(along.with = y)
  stats::approx(x = x, y = y, xout = x, rule = 2L)$y
}
