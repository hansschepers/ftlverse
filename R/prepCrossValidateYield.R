#' prepCrossValidateYield
#' @examples \dontrun{
#'   dtw <- data.table(dateTime =  seq.Date(as.Date("2021-01-01"), by = "week", length.out = 4), yield = 4)
#'   prepCrossValidateYield(dtw, bycols = character(0), toAdd = character(0), switchDate = "yield")
#'   prepCrossValidateYield(dtw, bycols = character(0), toAdd = character(0), switchDate = "none")
#'   prepCrossValidateYield(dtw, bycols = character(0), toAdd = character(0), switchDate = "today")
#'   prepCrossValidateYield(dtw, bycols = character(0), toAdd = character(0), switchDate = "2021-01-08")
#'   prepCrossValidateYield(dtw, bycols = character(0), toAdd = character(0), switchDateWksBack = 2)
#'   prepCrossValidateYield(dtw, bycols = character(0), toAdd = character(0), switchDateWksBack = 3)
#'   prepCrossValidateYield(dtw, bycols = character(0), toAdd = character(0), switchDateWksBack = 4)
#' }
#' @export
prepCrossValidateYield <- function(
  dtw
  , switchDateWksBack = 0
  , addRowsTopredict = 3
  , bycols = c("plot_syn", "cycle_syn")
  , switchDate = c("auto", "yield", "today", "none")[2]
  , pNout = c("stem.density.setting", "pruning"
              , "RAD", "ECIRRI", "GHCO2C", "GHTEMP", "OUTEMP")
  , toAdd = c("setting", "setting.cum"
              , "harvest", "harvest.by2", "harvest.by3", "harvest.cum"
              , "afw"
              , "yield", "yield.by2", "yield.by3", "yield.cum"
              , "yield_lz", "yield_lz3", "yield_lz5"
              , "yield_wkavg"
              , "harvestMaturity"
              , "harvestMaturityFromTemp"
              , "lightsum.day.years.out", "speedup"
              , "temp24hr"
              , "plantLoad"
              , "stem.diam"
              , "strength"
              , "trussSpeed"
              , "plantLoad_lz")
  , maxDate = ISOdatetime(2022, 12, 31, 0,0,0)
  , verbosity = 0
){
  dtw <- copy(dtw)
  stopifnot(!"processName" %in% names(dtw))
  
  # remember 'actual'
  {
    toAdd <- intersect(names(dtw), toAdd)
    keep <- c(bycols, "dateTime", toAdd)
    dtwActual <- copy(dtw)[, ..keep]
    iin <- names(dtwActual) %in% toAdd
    names(dtwActual)[iin] <- paste0(names(dtwActual)[iin], ".actual")
    .dtwActual <<- dtwActual
    dtwActual
  }
  
  # cut off at switchDate
  {  
    ############################################################## MELT!  
    DT <- aphMelt(dtw)
    if (is.character(switchDate)){
      switchDate <- switch(
        switchDate
        , auto = DT[!processName %in% pNout, hmax(dateTime)]
        , yield = DT[processName == "yield", hmax(dateTime)]
        , today = Sys.Date() + 6 #  6 days
        , none = maxDate
        , as.Date(switchDate)
      )
    }
    log_debug("switchDate1 {switchDate}")
    if (switchDateWksBack > 0){
      switchDate <- switchDate - lubridate::weeks(switchDateWksBack)
      if (verbosity > 0){
        log_success("taking switchDate from switchDateWksBack ({switchDateWksBack}): {switchDate}")
      }
    }
    # if (is.Date(switchDate)){
    log_info("cutting off beyond: {switchDate}")
    DT <- DT[dateTime <= switchDate]
    ############################################################## CAST!  
    dtw <- hdcast(DT)
  }
  
  # add dates to be be predicted
  # add the test set for future -- 
  if (addRowsTopredict > 0){
    dtwSplit <- split(dtw, by = c(bycols))
    # print(dtwSplit)
    dtwListCV <- lapply(dtwSplit, function(dtw){
      dtw[, timeHorizon := 0]
      # don't predict old cycles
      if (hmax(dtw$dateTime) >= switchDate){
        toPredict <- dtw[rep(NROW(dtw), addRowsTopredict)]
        toPredict[, dateTime := max(dateTime) + lubridate::weeks(1:addRowsTopredict)]
        toPredict[, (setdiff(names(toPredict), c("dateTime", bycols))) := NA]
        #TODO add weather prediction: Radiation --
        toPredict[, timeHorizon := .I]
        dtw <- rbind(dtw, toPredict)
      }
      dtw
    })
    dtw <- rbindlist(dtwListCV)
  }
  
  # join with dtwActual
  # .dtwBeforeJoin <<- copy(dtw)
  dtwCV <- dtwActual[dtw, on = c(bycols, "dateTime")]
  dtwCV[, switchDate := switchDate]
  dtwCV <- dtwCV[dateTime <= maxDate]
  aphKey(dtwCV, ignoreAsDois = c("wk", "weekno"))
  # with (prepPars,{
  #   dtwCV[, {if (okData(pruning) == 0) pruning := basePruning}, by = c(bycols)]
  # })
  
  dtwCV
}
