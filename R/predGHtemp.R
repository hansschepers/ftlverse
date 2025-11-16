#' predGHtemp
#' @description Get weather forecasts to estimate greenhouse temperatures and
#'   make yield predictions
#' @export
predGHtemp <- function(DT, predStartDate = Sys.Date()
                       , interval = 24*3600
                       ){
  # make model based on current data
  DTpred <- DT[processName %in% c("GHTEMP","RAD","OUTEMP")]
  DTpred <- dcast(DTpred, dateTime ~ processName)
  
  tempModel <- makeModel(DTpred
                         , ooi = "GHTEMP"
                         , ioi = c("OUTEMP","RAD")
                         , fn = e1071::svm)
  accountId <- unique(DT$account_id)
  
  weatherPred <- wfT[wfRad, on = "dateTime"]
  
  # set to proper time zone
  attr(weatherPred$dateTime,"tzone") <- tz(DTpred$dateTime)
  setnames(weatherPred,c("temperature","radiation"),c("OUTEMP","RAD"))
  
  weatherPred <- melt(weatherPred
                      , id.vars = "dateTime"
                      , variable.name = "processName")
  weatherPred <- reduceTimePoints(weatherPred
                                                 , interval = interval
                                                 , fun.aggregate = hmean
                                                 , byfois = "processName")
  
  weatherPred <- dcast(weatherPred
                       , dateTime ~ processName)[dateTime >= predStartDate]
  
  weatherPred[, GHTEMP := predict(tempModel, newdata = .SD)]
  
  weatherPred <- melt(weatherPred
                      , id.vars = "dateTime"
                      , variable.name = "processName"
                      , measure.vars = "GHTEMP"
                      , variable.factor = FALSE)
  weatherPred
}
