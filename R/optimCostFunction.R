#' optimCostFunction
#' @examples \dontrun{
#'   optimCostFunction(c(tempSpan = 11), dtwClean = dtwClean)
#'   
#'   tempSpan.s <- seq(4, 24, 2)
#'   gridSearch <- sapply(tempSpan.s, function(x) optimCostFunction(c(tempSpan = x), dtwClean = dtwClean))
#'   plot(x = tempSpan.s, y = gridSearch)
#'   
#'   simList <- lapply(tempSpan.s, function(x) simCV(c(tempSpan = x), dtwClean = dtwClean))
#'   ww <- lapply(simList, summarizeCV, doplot = TRUE)
#'   
#'   optimCostFunction(c(tempSpan = 11), dtwClean = dtwClean, doplot = TRUE)
#'   
#'   summarizeCV(simList[[6]])
#'   ww <- lapply(simList, summarizeCV)
#'   
#'   opop <- optimx::optimx(c(fldMix = .5), fn = optimCostFunction
#'                          , method = c('Nelder-Mead'), dtwClean = dtwClean)
#'    # maturityDegreeDays = 815, 
#'   opop <- optimx::optimx(c(maturityDegreeDays = 815, baseTemp = 6), fn = optimCostFunction
#'                          , method = c('Nelder-Mead'), dtwClean = dtwClean)
#' }
#' @export
optimCostFunction <- function(
  # args for simCV()
  parvec
  , dtwClean
  , enrichPars = getEnrichPars()
  , modelsGiven
  , switchDateWksBack.s = seq(8, 28, 5)
  , keepSimList = FALSE
  # args for summarizeCV()
  , kpis = "yield"
  , metric.s = c(hMAPE = hMAPE)#, hcor = hcor)
  , timeHorizon.s = 1 # 1:4
  , cropseason_id.s = "all"
  # args for extrapolateYield()
  , ...){
  
  simList <- simCV(parvec
                   , dtwClean = dtwClean
                   , enrichPars = enrichPars
                   , modelsGiven = modelsGiven
                   , switchDateWksBack.s = switchDateWksBack.s
                   , ...
                   )
  kpiList <- summarizeCV(simList
              , metric.s = metric.s
              , kpis = kpis
              , timeHorizon.s = timeHorizon.s
              , cropseason_id.s = cropseason_id.s
  )
  if (keepSimList){
    message("attaching simList")
    attr(kpiList, "simList") <- simList #list(simList = simList)
  }
  # return(kpiList)
  kpiList
}
