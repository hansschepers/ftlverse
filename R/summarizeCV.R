#' summarizeCV
#' @param metric.s named vector with functions that each evaluate numeric vectors 'truth' and 'estimate'
#' @param kpis character vector with processNames; if character(0) all processNames are kept
#' @export
summarizeCV <- function(simList
                        , metric.s = c(hMAPE = hMAPE)#, hcor = hcor)
                        , kpis = c("yield")
                        , timeHorizon.s = 1 # 1:4
                        , cropseason_id.s = "all"
                        , truthExtension = c(".actual", "")[1]
){
  performanceList <- lapply(simList
                            , summaryMetrics
                            , metric.s = metric.s
                            , byPerf = c("cropseason_id", "timeHorizon", "switchDate")
                            , truthExtension = truthExtension
  )
  
  allPerf <- rbindlist(performanceList, fill = TRUE, idcol = "sim")
  allPerf[, processName := sub(truthExtension, "", processName)]
  
  # selections
  if ((!"all" %in% cropseason_id.s)){
    allPerf <- allPerf[cropseason_id %in% cropseason_id.s]
  }
  if (all(timeHorizon.s %in% allPerf$timeHorizon)){
    allPerf <- allPerf[timeHorizon %in% timeHorizon.s]
  } else {
    log_error("not all these timeHorizons is not present in the data; not filtered")
  }
  .allPerf <<- copy(allPerf)
  
  perfProfile <- allPerf[, (hsummary(.estimate, hfuns = "ribbon"))
                         , by = c(voi, "timeHorizon", ".metric")]
  
  setnames(perfProfile, "hmean", "value")
  # str(perfProfile)
  aphKey(perfProfile)
  .perfProfile <<- perfProfile
  
  if (length(kpis) > 0) {
    perfProfile <- perfProfile[processName %in% kpis, ]
  }
  cost <- perfProfile[, hmean(value)]
  
  if (nrow(perfProfile) > 1){
    attr(cost, "perfProfile") <- copy(perfProfile)
  }
  attr(cost, "allPerf") <- copy(allPerf)
  cost
}
