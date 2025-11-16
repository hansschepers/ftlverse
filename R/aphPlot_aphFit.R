#' hPlot.aphFit
#' @export
hPlot.aphFit <- function(fitList
                           , weekCut = 30, ...){
  ww <- summary(fitList$fit2p)
  DT2 <- fitList$DT[!is.na(harvest.maturity)]
  DT2$pred <- predict(fitList$fit2p)
  if (!"weekno" %in% names(DT2)){
    DT2[, weekno := week(dateTime)]
  }
  DT2[, weekGroup := ifelse(weekno < weekCut, "early", "late")]
  p2p <- pggs(DT2, xoi = "Tavg", yoi = "harvest.maturity"
              , foi = "weekGroup", label = "weekno"
              , geom = "pointline", lwd = .1, mega = TRUE
              , doplot = FALSE)
  p2pcc <- pggs(DT2, xoi = "Tavg", yoi = "pred"
                , foi = "weekGroup"
                , p = p2p
                , fsize = 16
                , lwd = 4, lineAlpha = .3, lineColor = "green"
                , ...
  )
  return(p2pcc)
}
