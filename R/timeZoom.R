#' timeZoom
#' @examples \dontrun{
#'   x = c(NA, 1, 8, 3)#[-1]
#'   xx <- timeZoom(x, zoom = 7, scaleDown = 7)
#'   plot(xx)
#'   hprettyNum(xx)
#' }
#' @export
timeZoom <- function(x = c(NA, 1, 8, 3)[-1]
                     , zoom = 4
                     , scaleDown = zoom
                     , ...){
  if(zoom == 1) return(x)
  xtotal <- hsum(x)
  if (zoom != scaleDown){
    xtotal <- xtotal * zoom / scaleDown
  }
  y <- lapply(x, function(z) {
    if(is.na(z)) z <- 0
    rep(z, zoom) / scaleDown
  })
  y <- unlist(y)
  for(i in seq_len(zoom-1)){
    y <- hfrollmean(y, align = "center")
  }
  ytotal <- hsum(y)
  kpi <- (ytotal / xtotal - 1)
  if (is.na(kpi)) return(y)
  
  if (abs(kpi) > 0.000001){
    message("totals changed")
    print(c(ytotal, xtotal, kpi))
  }
  y
}
