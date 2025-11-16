#' plotPerfSummary
#' 
#' @export
plotPerfSummary <- function(dtwExtra# = ddl
                        , bycols = c("plot_syn", "cycle_syn")
                        , byPerf1 = c(bycols)#, "weeknoFactor")
                        , truthExtension = ""){
  
  dtm <- aphMelt(dtwExtra)[grepl("~", processName)]
  
  d1 <- hdcast(dtm)[, summaryMetrics(.SD
                                     , predExtension = "\\.pred$"
                                     , truthExtension = truthExtension)
                    , by = byPerf1
                    ]
  
  d1[, processName := sub("~", "\nModel:", processName)]
  
  p0 <- pggs(d1, yoi = voi, xoi = ".estimate", foi = foip, facet_w = "nothing"
             , geom = "point", psize = 6, pointAlpha = .4
             , label = foip, labelAngle = 45)
  
  d1summ <- d1[, hsummary(.estimate), by = c(voi)]
  p1 <- pggs(d1summ, p = p0, yoi = voi, xoi = "hmean", facet_w = "nothing"
             , geom = "point", psize = 14, pointAlpha = 1)
  # dashed lines 
  p2 <- pggs(d1, p = p1, yoi = voi, xoi = ".estimate", facet_w = "nothing"
             , foi = "nothing", group = foip, linetype = 2)
  
  pggs(d1summ, p = p2, yoi = voi, xoi = "hmean", facet_w = "nothing"
       , xmin = "hmin", xmax = "hmax", geom = "errorhbar", lwdEB = 2
       , projector = TRUE)
}
