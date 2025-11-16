#' zigzag
#'
#' descending piecelinear zig zag curve
#'
#' @examples \dontrun{
#'   plot(zigzag(x=1:100, thr = 20, sat = 90), pch = 20)
#'   abline(v=55)
#'   points(shiftedHill(x = 1:100, thr = 55, expo = 2, max2thr = 55), col = 2)
#'   points(shiftedHill(x = 1:100, thr = 55, expo = 3, max2thr = 35), col = 3)
#'   points(1 - logistXY(x = 1:100, x0 = 55, y0 = .5, r=.1), col = 4)
#'   points(pwl(x = 1:100, thr = 55, lbSlope = -1/70, ubSlope = -1/70, yref = 0.5), col = 5)
#'   points(pwl(x = 1:100, thr = 75, lbSlope = 1/70, ubSlope = -1/7, yref = 0.8), col = 6, cex = .5)
#'   points(pwl(x = 1:100, thr = 75, lbSlope = -1/70, ubSlope = 1/7, yref = 0.8), col = 7, cex = .5)
#'   points(pwl(x = 1:100, thr = 35, lbSlope = .1, ubSlope = -.1))
#' }
#'
#' @export
zigzag <- function(x, thr, sat, ymin = 0, ymax = 1){
  pmin(pmax((x - sat) / (thr - sat), ymin), ymax)
}

#' logistXY
#' 1 / (1 + (1 / y0 - 1) * exp(-r*(x-x0)))
#' @examples \dontrun{
#'   plot(lstNE(x=seq(-1, 10, 1), y=logistXY(x, r=-1, ymax=1, y0=.5, x0 = 4))); points(x=4, y=.5, pch = 20)
#'   plot(lstNE(x=seq(-1, 10, 1), y=logistXY(x, r=1, ymax=1, y0=.5, x0 = 4))); points(x=4, y=.5, pch = 20)
#'   plot(lstNE(x=seq(-1, 10, 1), y=logistXY(x, r=1, ymin = .6, ymax=.8, y0=.7, x0 = 4))); points(x=4, y=.7, pch = 20)
#'   plot(lstNE(x=seq(-4, 10, 1), y=logistXY(x, r=4, ymax=1, y0=.95, x0=0)), ylim = c(0, 1)); points(x=0, y=.95, pch = 20)
#'   plot(lstNE(x=seq(-4, 10, 1), y=logistXY(x, r=4, ymin = 0.6, ymax=0.8, y0=.75, x0=0)), ylim = c(0, 1)); points(x=0, y=.75, pch = 20)
#'   plot(lstNE(x=seq(-4, 10, 1), y=logistXY(x, r=0, ymax=1, y0=  1, x0=0)), ylim = c(0, 1)); points(x=0, y=  1, pch = 20)
#'   plot(lstNE(x=seq(-4, 10, 1), y=logistXY(x, r=1, ymin = 130, ymax=220, y0=180, x0=1))); points(x=1, y=180, pch = 20)
#'   plot(lstNE(x=seq(-4, 10, 1), y=logistXY(x, r=1, ymin = 220, ymax=130, y0=180, x0=1))); points(x=1, y=180, pch = 20)
#'   plot(lstNE(x=seq(-4, 10, 1), y=logistXY(x, r=0, ymin = 130, ymax=220, y0=180, x0=1))); points(x=1, y=180, pch = 20)
#' }
#' @export
logistXY <- function(x, r = 1, x0 = 0, y0 = 0.5, ymax = 1, ymin = 0){
  if (abs(r[1]) < 1e-4) {
    return(rep(y0, length(x)))
  }
  if (any(y0 < ymin)){
    message("help, y0 < ymin (logistXY)")
  }
  # ymin + (ymax - ymin) / (1 + ((ymax-ymin) / pmax(0, y0 - ymin) - 1) * exp(-r*(x-x0)))
  ymin + (ymax-ymin) / (1 + ((ymax-ymin)/(y0-ymin) - 1) * exp(-r*(x-x0)))
}


logistXY_doc <- function(x, r = 1, x0 = 0, y0 = 0.5, ymax = 1, ymin = 0){
  ymin + (ymax-ymin) / (1 + ((ymax-ymin)/(y0-ymin) - 1) * exp(-r*(x-x0)))
}

#' shiftedHill
#'
#' descending Sigmoid 'Hill' curve, parametrized
#' y = 1 / (1 + (x/thr)^expo)
#' implemented to allow for negative thr threshold (midpoints)
#' @param x X value, can be negative!
#' @param thr midpoint / threshold value, can be negative!
#' @param expo exponent
#' @param max2thr allows for X-axis scaling
#'
#' @examples \dontrun{
#'   library(data.table)
#'   dt <- CJ(x = seq(-5, 5, .25), thr = c(-2, 2), expo = c(1, 3, 6), max2thr = c(1, 4))
#'   dt[, y := shiftedHill(x, thr, expo=3, max2thr = max2thr)]
#'   dtm <- melt(dt, id.vars = c("x", "thr", "expo", "max2thr"))
#'   str(dtm)
#'   dtm[, variable := as.numeric(variable)]
#'   #library(aphLite)
#'   p <- pggs(dtm, foi = "max2thr", facet_w = "thr"
#'             , lwd = 2, xtics = 1, doplot = TRUE
#'             , vline = c(-2, 0, 2), hline = 0)
#'
#'   # subtle differences
#'   dt2 <- CJ(variant = c("expo=03, max2thr=1", "expo=18, max2thr = 4"), x = seq(0, 14, .1))
#'   dt2[as.numeric(as.factor(variant)) == 1, value := shiftedHill(x, thr=2, expo=3, max2thr = 1)]
#'   dt2[as.numeric(as.factor(variant)) == 2, value := shiftedHill(x, thr=2, expo=18, max2thr = 4)]
#'   p <- pggs(dt2, foi = "variant", vline = 2, doplot = TRUE, title = "Subtle variants of Sigmoid curves")
#'   x <- seq(0, 14, .1)
#'   dd <- shiftedHill(x, thr=2, expo=3, max2thr = 1) - shiftedHill(x, thr=2, expo=18, max2thr = 4)
#'   plot(dd) ; abline(h=0)
#'   plot(dd>0) ; abline(h=0)
#'
#'   dt2 <- CJ(variant = c("expo=03, max2thr=1", "expo=18, max2thr = 3"), x = seq(0, 14, .1))
#'   dt2[as.numeric(as.factor(variant)) == 1, value := shiftedHill(x, thr=2, expo=3, max2thr = 1)]
#'   dt2[as.numeric(as.factor(variant)) == 2, value := shiftedHill(x, thr=2, expo=18, max2thr = 2)]
#'   p <- pggs(dt2, foi = "variant", vline = 2, doplot = TRUE, title = "Subtle variants of Sigmoid curves")
#'   x <- seq(0, 14, .1)
#'   dd <- shiftedHill(x, thr=2, expo=3, max2thr = 1) - shiftedHill(x, thr=2, expo=18, max2thr = 2)
#'   plot(dd) ; abline(h=0)
#'   plot(dd > 0) ; abline(h=0)
#' }
#' @export
shiftedHill <- function(x, thr, expo, max2thr = abs(thr)){
  1 / (1 + pmax(x/max2thr - (thr/max2thr - 1), 0)^expo)
}


#' pwl
#'
#' piece linear lines around a midpoint, with ymin and ymax, optionally different left and right
#' @param x   X value
#' @param thr X value of thr midpoint, reference point
#' @param lbSlope slope for x < thr
#' @param ubSlope slope for x >= thr
#' @param yref Y value at x = thr
#' @param ymin lowest possible Y value (for x < thr only if yminRight is specified)
#' @param ymax highest possible Y value (for x < thr only if ymaxRight is specified)
#' @param yminRight lowest Y value for x >= thr
#' @param ymaxRight highest Y value for x >= thr
#' @examples \dontrun{
#'   plot(  pwl(x = 1:100, thr = 55, lbSlope = 0.1, ubSlope = -0.02), col = 6)
#'   points(pwl(x = 1:100, thr = 55, lbSlope = 0.1, ubSlope = -0.02, yref = 0.5), col = 2)
#'   points(pwl(x = 1:100, thr = 65, lbSlope = 0.1, ubSlope = 0.02, yref = 0.5, ymin = 0.1, ymax = 0.8), col = 3)
#'   points(pwl(x = 1:100, thr = 65, lbSlope = 0.1, ubSlope = 0.02, yref = 0.7, ymin = 0.1, ymax = 0.8), col = 3)
#'   points(pwl(x = 1:100, thr = 65, lbSlope = 0.1, ubSlope = -0.02, yref = 0.5, ymin = 0.2, yminRight = 0), col = 4)
#' }
#' @export
pwl <- function(x, thr = 1, lbSlope = 1, ubSlope = -1, yref = 1
                , ymin = 0, yminRight = ymin
                , ymax = 1, ymaxRight = ymax){
  slope <- ifelse(x < thr, lbSlope, ubSlope)
  ymin <- ifelse(x < thr, ymin, yminRight)
  ymax <- ifelse(x < thr, ymax, ymaxRight)
  pmin(ymax, pmax(ymin, slope * (x - thr) + yref))
}
