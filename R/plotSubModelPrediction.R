#' plotSubModelPrediction
#' 
#' @export
plotSubModelPrediction <- function(dtw
                                   , yoi = "harvestMaturity"
                                   , yoiPred = paste0(yoi, ".pred")
                                   , input = list(facet_w = "plot_syn"
                                                  , free_x = TRUE
                                                  , free_y = FALSE
                                                  , title = yoi
                                                  # , ysc = c(4, 14)
                                                  # , label = "wkf"
                                                  )
                                   , byCols = c("plot_syn")
                                   , sep = c("~", "__")[1]
                                   , ...
                                   ){
  kk <- c(bycols, "dateTime", yoi, yoiPred)
  dtw <- copy(dtw)[, ..kk]
  dtw[, yr := lubridate::year(dateTime)]
  # dtw[, wk := isoweek(dateTime)]
  dtw[, wk := lubridate::week(dateTime)]
  
  weekModulo.s <- dtw[0 == wk%%10, dateTime]
  
  p <- ppggs(dtw, yoi = yoi
             , input = input
             , pointAlpha = .3
             , ...)
  p2 <- pggs(dtw, p = p, yoi = yoiPred
             , lwd = 1.5, lineAlpha = .6, lineColor = "darkgrey"
             , input = input
             , vline = weekModulo.s
             , ablinecolor = "yellow", lwdFit = 2, lineAlphaFit = .2
             , ...
  )
  p2
}


#' plotXySubModelPrediction
#' 
#' @export
plotXySubModelPrediction <- function(dtw
                                   , yoi = "harvestMaturity"
                                   , yoiPred = paste0(yoi, ".pred")
                                   , foi = "wkGroup"
                                   , facet_w = "plot_syn"
                                   # , modelId = ""
                                   , input = list(fsize = 12, xtics = 1, ytics = 1, grid = TRUE
                                                  , free_y = FALSE, abline = c(0, 1)
                                                  , geom = "point"
                                                  , xlab = "Actual", ylab = "Predicted"
                                                  , title = yoi
                                                  , ci.alpha = .1
                                   )
                                   , byCols = c("plot_syn")
                                   , sep = c("~", "__")[1]
                                   , wkModulo = 18
                                   , ...
){
  dtw <- copy(dtw)
  dtw[, yr := lubridate::year(dateTime)]
  # dtw[, wk := lubridate::isoweek(dateTime)]
  dtw[, wk := lubridate::week(dateTime)]
  dtw[, wkGroup := floor(wk/wkModulo)]
  dtw[, wkf := ifelse (0 == wk%%10, prettyNum(wk, width = 2), "")]
  pggs(dtw
       , xoi = yoi
       , yoi = yoiPred
       , foi = foi
       , facet_w = facet_w
       , input = input
       , ...
       )
}