#' scenVary
#' @examples \dontrun{
#'   scenDT = CJ(fruitFall = seq(0, 0.04, 0.02)
#'     , maxSetting = seq(10, 18, 4)
#'     , trussTempSpan = seq(1, 5, 2)
#'     , maturityDegreeDays = seq(900, 1300, 50) )
#'   scenDT
#'   log_threshold(SUCCESS)
#'   system.time({scenDT <- scenVary(scenDT)})
#'   .scenDT[cost <= 1.2*hmin(cost)]
#'   
#'   pggs(.scenDT
#'   , xoi = "fruitFall", yoi = "cost"
#'     , foi = c("maxSetting", "trussTempSpan"), facet_w = "sf"
#'     , geom = "pointline", psize = 6, free_y = FALSE, fsize = 14, ysc = c(0, .5))
#'   
#'   pggs(.scenDT[baseTemp == 7]
#'   , xoi = "fruitFall", yoi = "cost"
#'     , foi = c("maturityDegreeDays"), facet_w = "maxSetting"
#'     , geom = "pointline", psize = 6, free_y = FALSE, fsize = 14, ysc = c(0, .5))
#'  # write.csv(scenDT, "../scenDT_swdwk8_18_28.csv")
#' }
#' @param scenDT a 'scenario' data.table, made by CJ()
#' @returns a data.table, same as scenDT, with one extra column
#' @export
scenVary <- function(scenDT
                     , dtwClean
                     , enrichPars = getEnrichPars()
                     , modelsGiven = list()
                     , switchDateWksBack.s = seq(8, 28, 10)
                     , keepSimList = TRUE
                     # for summarizeCV
                     , kpis = "yield"
                     , metric.s = c(hMAPE = hMAPE)#, hcor = hcor)
                     , timeHorizon.s = 1 # 1:4
                     , plot_syn.s = "all"
){
  # .scenDT <<- scenDT
  nn <- nrow(scenDT)
  scenDT[, ind := seq(.N)]
  # scenDT[, iter := paste0(seq(nn), "/", nn)]
  ii <- 2
  for (ii in seq(nn)){
    gc()
    parvec <- unlist(scenDT[ii])
    kpiList <- optimCostFunction(parvec
                             , dtwClean = dtwClean
                             , enrichPars = enrichPars
                             , modelsGiven = modelsGiven
                             , switchDateWksBack.s = switchDateWksBack.s
                             , keepSimList = keepSimList
                             # for summarizeCV
                             , metric.s = metric.s
                             , kpis = kpis
                             , timeHorizon.s = timeHorizon.s
                             , plot_syn.s = plot_syn.s
    )
    scenDT[ii, cost := kpiList]
    cat(paste(ii, nn, sep = " / ")) ; print(unlist(scenDT[ii, ]))
  }
  scenDT
}
