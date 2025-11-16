#' @title
#' Linear Extrapolation
#'
#' @description
#' A simple function to linearly extrapolate a single input single output relation, The `x` and `y` vectors are used to
#' retrieve a linear model. The new data in `xout` is used to to retrieve the new output.
#'
#' @param x `numeric()`. The input vector to construct the linear model
#' @param y `numeric()`. The output vector for the linear model
#' @param xout `numeric()`. A vector of data to be used in the prediction.
#'
#' @examples
#' \dontrun{
#' x <- seq(1000, 1020)
#' y <- seq(100, 200, length.out = 21)
#' xout <- seq(1021, 1025)
#' linExtrap(x, y, xout)
#' }
#'
#' @return The linearly extrapolated y values
#'
#' @importFrom stats lm predict na.exclude
#' @export
linExtrap <- function(x = seq_along(y)
                      , y
                      , xout
) {
  if (length(x) != length(y)) {
    logger::log_error("linExtrap(): Input and output vectors x and y are not of the same length.")
    stop()
  }
  dat <- data.frame(x = x, y = y)
  newdat <- data.frame(x = xout)
  linMod <- lm(y ~ x, data = dat, na.action = na.exclude)
  yout <- predict(linMod, newdata = newdat)
  return(yout)
}

#' padLinear
#' @examples \dontrun{
#'   y = c(6, 8, 10, 12,14, 16)
#'   padLinear(y = y)  # default: zero padding left and right
#'   padLinear(y = y, n = c(2, 4))
#'   padLinear(y = c(6, 8, 10, 12, NA, 16), n = c(2, 4))
#'   padLinear(y = c(y, NA), n = c(2, 4))
#' }
#' @export
padLinear <- function(y
                      , x = seq_along(y)
                      , n = c(0, 0)
                      , rule = 2){
  if (sum(n) <= 0) return(y)
  y <- aphApprox2(y, rule = rule)
  if (n[1] > 0){
    leftPad <- linExtrap(x, y, seq(-n[1]+1, 0, 1))
  } else {
    leftPad <- numeric()
  }
  if (n[1] > 0){
    rightPad <- linExtrap(x, y, max(x)+seq_len(n[2]))
  } else {
    rightPad <- numeric()
  }
  res <- c(leftPad, y, rightPad)
  unname(res)
}
