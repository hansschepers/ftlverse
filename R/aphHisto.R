#' aphHisto
#' 
#' @export
aphHisto <- function(x
                     , dx = 0
                     , histoMin = hmin(x)
                     , histoMax = hmax(x)
                     , breaks = "auto"
                     , doplot = FALSE){
  histoMin <- min(histoMin, hmin(x))
  histoMax <- max(histoMax, hmax(x))
  histoMin <- histoMin - (histoMax - histoMin)/10
  histoMax <- histoMax + (histoMax - histoMin)/10
  if (dx == 0){
    length.out <- ceiling(sqrt(length(x)))
    dx <- (histoMax - histoMin) / length.out
  } else {
    length.out <- 1 + (histoMax - histoMin) / dx
  }
  if (is.character(breaks[1])){
    breaks <- seq(from = histoMin, to = histoMax, length.out = length.out)
  }
  log_debug("histoMin {histoMin}, histoMax {histoMax}, dx {breaks[2] - breaks[1]}")
  # print(breaks)
  hh <- hist(x
             , breaks = breaks
             , plot = doplot
  )
  hh$dx <- dx
  hh$density <- hh$density * dx
  hh$counts <- as.numeric(hh$counts)
  hh
}
