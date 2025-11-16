#' simCV
#' @examples \dontrun{
#'  simCV(dtwClean = dtwClean)
#' }
#' 
#' @export
simCV <- function(parvec = c(dummy = 0)
                  , enrichPars = getEnrichPars()
                  , dtwClean
                  , modelsGiven = list()
                  , bycols = c("cycle_syn", "plot_syn")
                  , addRowsTopredict = 4
                  , switchDateWksBack.s =  12
                  , ...
){
  # enrichPars[[names(parvec)]] <- parvec
  enrichPars <- mergeParameters(enrichPars, parvec)
  # enrich __once__ ' for Truth' to add enriched variables (harvestMaturity etc) ----
  dtwCV <- prepCrossValidateYield(dtwClean                  # <-- ################
                                  , switchDateWksBack = 0   # <-- ################
                                  , addRowsTopredict = addRowsTopredict)
  .dtwCV <<- dtwCV
  {
    dtwExtra <- extrapolateYield(dtwClean
                                 , bycols = bycols
                                 , modelsGiven = modelsGiven
                                 , enrichPars = enrichPars
                                 , ...)
    (kk <- grep("\\.pred$", names(dtwExtra), value = TRUE))
    dtwExtra[, (kk) := NULL]
    dtwExtra[, timeHorizon := NULL]
  }
  
  {
    simList <- list() ; ii <-  0
    switchDateWksBack <- switchDateWksBack.s[1]
    for (switchDateWksBack in switchDateWksBack.s){
      dtwCV <- prepCrossValidateYield(dtwExtra ################
                                      , switchDateWksBack = switchDateWksBack
                                      , addRowsTopredict = addRowsTopredict
      )
      dtwPredicted <- extrapolateYield(dtwCV
                                       , bycols = bycols
                                       , modelsGiven = modelsGiven
                                       , enrichPars = enrichPars
                                       , ...)
      DTpred4perf = copy(dtwPredicted)
      # merge with scenDT
      DTpred4perf$switchDateWksBack <- switchDateWksBack
      # switchDate <- DTpred4perf[timeHorizon == 0, max(dateTime)]
      ii <- ii + 1
      simList[[ii]] <- DTpred4perf
    }
  }
  return(simList)
}
