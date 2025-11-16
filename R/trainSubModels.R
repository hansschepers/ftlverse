#' trainSubModels
#' @examples \dontrun{
#'   modelsGiven <- trainSubModels(dtwClean, toTrain = c("rad", "temp", "trussFromPars")[1:3])
#'   modelsGiven
#'   modelsGiven <- trainSubModels(dtwClean, modelsGiven = modelsGiven
#'                               , toTrain = c("rad", "temp", "truss")[1:3])
#' }
#' 
#' @export
trainSubModels <- function(dtwClean
                           , trainingPlots = c("all", "k_20_m"
                                               , "k_21_m1", "k_21_m2", "k_21_m3"
                                               )[-1]
                           , maxDate = ISOdatetime(2022, 1, 1, 12, 0, 0)
                           , enrichPars = getEnrichPars()
                           , prepPars = getPrepPars()
                           , tryOUTEMP = FALSE
                           , bycols = intersect(names(dtwClean), c("plot_syn", "cycle_syn", "cropseason_id")) # only needed when making avtemp3
                           , modelsGiven = list()
                           , toTrain = c("rad", "tempFromPars", "trussFromPars")
){
  dtwCleanTraining <- copy(dtwClean)
  if (!"all" %in% trainingPlots){
    dtwCleanTraining <- dtwCleanTraining[plot_syn %in% trainingPlots]
  }
  dtwCleanTraining <- dtwCleanTraining[dateTime <= maxDate]
  # dtwCleanTraining[, .N, by = plot_syn]
  
  if ("rad" %in% toTrain){
    modelsGiven$radModel <- fitRAD(dtwCleanTraining)
  }
  
  
  if ("temp" %in% toTrain){
    modelsGiven$tempModel <- fitTEMP(dtwCleanTraining
                                     , modelsGiven
                                     , tryOUTEMP = tryOUTEMP)
  }
  if ("tempFromPars" %in% toTrain){
    modelsGiven$tempModel <- with(enrichPars,
                                  lm(temp24hr ~ light.sum.total.day
                                     , data = data.frame(light.sum.total.day = c(0, 1)
                                                         , temp24hr = c(temp.night, temp.night + RTR.c))
                                     , na.action = na.exclude))
  }
  
  
  # we need the response trussSpeed, misuse the full function to do this Feature Eng
  if ("truss" %in% toTrain){
    if (! "avtemp3" %in% names(dtwCleanTraining)){
      dtwCleanTraining[, avtemp3 := hfrollmean(temp24hr, align = "right"), by = bycols]
    }
    if (all(c("trussSpeed", "avtemp3") %in% names(dtwCleanTraining))){
      # dtwCleanTraining <- extrapolateYield(dtwCleanTraining
      #                              , modelsGiven = modelsGiven
      #                              , enrichPars = enrichPars
      #                              , prepPars = prepPars
      #                              , trussTempSpan = 3
      #                              , maxSetting = 100
      #                              , trussMix = 0)
      dtwCleanTraining[trussSpeed < prepPars$minTrussSpeed, trussSpeed := NA]
      dtwCleanTraining[trussSpeed > prepPars$maxTrussSpeed, trussSpeed := NA]
      modelsGiven$trussSpeedModel <- with(dtwCleanTraining
                                          , lm(trussSpeed ~ avtemp3
                                               , na.action = na.exclude)
      )
    }
  }
  
  if ("trussFromPars" %in% toTrain){
    if (! "avtemp3" %in% names(dtwCleanTraining)){
      dtwCleanTraining[, avtemp3 := hfrollmean(temp24hr, align = "right"), by = bycols]
    }
    modelsGiven$trussSpeedModel <- with(enrichPars,
                                        lm(trussSpeed ~ avtemp3
                                      , data = data.frame(avtemp3 = c(temp.min.truss, temp.min.truss + 1)
                                                          , trussSpeed = c(0, truss.accell))
                                      , na.action = na.exclude)
    )
  }
  
  modelsGiven
}
