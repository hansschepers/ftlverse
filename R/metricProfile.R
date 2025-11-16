#' metricProfile
#' 
#' @export
metricProfile <- function(perfProfile
                           , input = list(xoi = "value", yoi = voi
                                          , foi = "timeHorizon"
                                          , facet_w = "nothing"
                                          , fsize = 14, ylab = NULL, xlab = "MAPE"
                                          , geom = "pointline"
                                          # , xsc = c(0, 0.5)
                                          , legend = "top"
                                          , relsize = c(.5, 1)
                                          , title = "prediction Performance Profile" 
                                          , vline = 0.1
                                          # give from parent env: paste("hMAPE", parmDiff, list2title(enrichPars), sep = "\n\n")
                                          )
                           , voi = "processName"
                           , kpis = "yield"
                           , ...
                           # , foip = "plot_syn"
){
  aphKey(perfProfile)
  p0 <- pggs(perfProfile, input = input, psize = 6, doplot = FALSE, allowppt = FALSE, ...)
  .p00 <<- p0
  if (!length(kpis)) {
    return(p0)
  }
  p <- pggs(perfProfile[processName %in% kpis]
            , p = p0, input = input
            , psize = 26, pointAlpha = .3
            , ...)
  return(p)
}
