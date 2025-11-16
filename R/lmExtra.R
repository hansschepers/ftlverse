#' lmExtra
#' @examples \dontrun{
#'   lmExtra(x0 = c(1, 2, 3, 4, NA, NA), span = 2)
#'   lmExtra(x0 = c(1, 2, 3, 4), span = 2, timeHorizon = 2)
#'   lmExtra(x0 = c(1, 2, 3, 4), span = 2, timeHorizon = 12)
#'   lmExtra(x0 = c(1, 2, 3, 4), span = 2)
#'   x0 <- c(NA, NA, NA, NA, 1, 5, NA, 7, 5, NA, NA, NA)
#'   lmExtra(x0)
#'   lmExtra(x0, span = 5)
#'   lmExtra(c(x0, rep(NA, 6)))
#'   lmExtra(c(x0, rep(NA, 9)))
#'   x0 <- c(NA, NA, NA, NA, NA, 5, NA, NA, NA)
#'   lmExtra(x0)
#'   lmExtra(1, span = 1, timeHorizon = 4)
#'   lmExtra(1, span = 2, timeHorizon = 4)
#'   lmExtra(c(1, NA), span = 2, timeHorizon = 1)
#' }
#' @export
lmExtra <- function(x0, span = 4, timeHorizon = 0){
  stopifnot(span <= hlength(x0))
  x0 <- c(x0, rep(NA, timeHorizon))
  nn <- length(x0)
  x <- fillInternalNAs(x0)
  rightNAs <- firstNonNA(rev(x)) - 1
  stopifnot(rightNAs > 0)
  to <- nn - rightNAs
  from = to - span + 1
  dd <- data.frame(x = seq(from, nn, 1), y = x[from:nn])
  dd
  fit <- lm(y ~ x, data = dd)
  yh <- predict(fit, newdata = dd)
  x0[seq(to + 1, nn, 1)] <- yh[seq(span+1, span+rightNAs, 1)]
  x0
}
