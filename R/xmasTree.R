#' xmasTree
#' @examples \dontrun{
#'   dt <- data.table(FN = c(21, 15, 5), h = c(50, 150, 210))
#'   dtxm <- xmasTree(dt, doplot = "ggplot")
#'   p <- pggs(dtxm, geom = "point", xoi = "fruits", yoi = "h", psize = "sz", ysc = c(0, 300), doplot = TRUE)
#'   p + scale_size(range = c(1, 22))
#' }
#' @export
xmasTree <- function(dt
                     , x = "FN"
                     , y = "h"
                     , doplot = "none"
                     , pggInput = list(xoi = "fruits", yoi = "h", psize = 1
                       , vline = 0, lwdFit = 3, ablinecolor = "darkgreen"
                     ), ...
){
  res <- list()
  ii <- 1
  for (ii in seq(nrow(dt))){
    fruits <- as.numeric(dt[ii, ..x])
    h <- as.numeric(dt[ii, ..y])
    res[[ii]] <- data.table(itime = ii
                            , fruits = c(seq(floor(fruits/2)) - 0.5, 
                                -seq(ceiling(fruits/2)) + 0.5
                              )
                            , h = h)
  }
  xmas <- rbindlist(res, fill = TRUE)
  xmas[, sz := (400/h)^2]
  # xmas[, color := (400/h)^2]
  if (doplot == "plotly"){
    xmasP <- plotly::plot_ly(xmas, x = ~fruits, y = ~h
                             , text = ~h
                             , type = 'scatter', mode = 'markers',
                             marker = list(size = ~sz, opacity = 0.5, color = ~h, color = "Reds"))
    print(xmasP)
    xmasP <<- xmasP
  }
  if (doplot == "ggplot"){
    xmasP <- ggplot() + geom_point(data=xmas, aes(x=fruits, y=h, size=sz, alpha=.8), color="red")
    xmasP <- pggs(xmas, p = xmasP
              , geom = "point"
              , input = pggInput, ...)
    print(xmasP)
    xmasP <<- xmasP
  }
  xmas[]
}