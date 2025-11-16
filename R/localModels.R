#' localModels
#' @examples \dontrun{
#'   localModels()
#'   localModels(dtwClean)
#' }
#' @export
localModels <- function(dtwClean
                        , bycols = c("plot_syn", "cycle_syn")
                        , yois = c("yield", "afw", "harvest", "plantLoad")[1]
){
  keep <- c(bycols, doi, yois)
  if (missing(dtwClean)){
    dtw <- data.table(plot_syn = "plot1", cycle_syn = "yr21"
                      , dateTime = ISOdate(2021, 1, 1) + lubridate::weeks(1:11)
                      , yield = c(0, 0, 0, 1,2,3, 3,4,2, NA, NA))
  } else {
    dtw <- dtwClean[, ..keep]
  }
  
  {
    dtw <- prepCrossValidateYield(dtw
                                  , switchDateWksBack = 6
                                  , addRowsTopredict = 3
                                  , bycols = c("plot_syn", "cycle_syn")
                                  , switchDate = "yield")
    dtw
    n1 <- names(dtw)
    
    # dtw[, yield_lz3.pred := lzman(get(yoi), 3), by = c(bycols)]
    # dtw[, yield_lz5.pred := lzman(get(yoi), 5), by = c(bycols)]
    # dtw[, yield_wkavg.pred2 := NULL]
    # dtw[, yield_lmextra4.pred2 := lmExtra(yield), by = bycols]
    
    dtw <- addBaseModelPrediction(dtw
                                  , modelId = "lz"
                                  , FUN = lzman
                                  , yois = yois
                                  , modelArgs = list(n = 1)
                                  , bycols = bycols
    )
    dtw <- addBaseModelPrediction(dtw
                                  , modelId = "lz3"
                                  , FUN = lzman
                                  , yois = yois
                                  , modelArgs = list(n = 3)
                                  , bycols = bycols
    )
    dtw <- addBaseModelPrediction(dtw
                                  , modelId = "lz5"
                                  , FUN = lzman
                                  , yois = yois
                                  , modelArgs = list(n = 5)
                                  , bycols = bycols
    )
    dtw[, weekno := lubridate::isoweek(dateTime)]
    dtw <- addBaseModelPrediction(dtw
                                  , modelId = "wkavg"
                                  , FUN = hmean
                                  , yois = yois
                                  , modelArgs = list()
                                  , bycols = "weekno"
    )
    dtw <- addBaseModelPrediction(dtw
                                  , modelId = "lmextra4"
                                  , FUN = lmExtra
                                  , yois = yois
                                  , modelArgs = list(span = 4)
                                  , bycols = bycols
    )
    n2 <- names(dtw)
    print(setdiff(n2, n1))
    print(dtw[])
    perf <- summaryMetrics(dtw
                   , byPerf = c("timeHorizon")
                   , metric.s = list(hMASE = hMASE
                                     , hMAPE = hMAPE
                                     # , n = length
                                     , hn = hlength2
                                     ))
    truthExtension <- "\\.actual"
    perf[, processName := sub(truthExtension, "", processName)]
    hdcast(perf, c("timeHorizon", voi), rhs = ".metric", value.var = ".estimate")
  }
}

