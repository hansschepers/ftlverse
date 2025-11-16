extrapolateYieldSummary2 <- function(
  dtw
  , modelsGiven = list()
  , daysPerRow = 7
  , enrichPars 
  , ...
){
  # drivers of temp
  dtw[, light.sum.total.day := predict(modelsGiven$radModel
                                       , newdata = data.table(weekno = weekno))]
  dtw[, OUTEMP := predict(modelsGiven$OUTEMPModel
                          , newdata = data.table(weekno = weekno))]
  # temp
  dtw[, temp24hr := predict(modelsGiven$tempModel
                            , newdata = data.table(light.sum.total.day = light.sum.total.day
                                                   , OUTEMP = OUTEMP)  )]
  # rolling means
  dtw[, avtemp3 := frollmeanMirror(temp24hr, n = 3)]
  dtw[, avtemp9 := frollmeanMirror(temp24hr, n = 9)]
  
  # setting  
  dtw[, trussSpeed := predict(modelsGiven$temp2truss
                              , newdata = data.table(avtemp3 = avtemp3))]
  dtw[, setting := trussSpeed * pruning * stem.density.setting]
  dtw[, setting := setting * (1 - fruitFall*(.25 + weekno/10))]
  dtw[week(dateTime) > headRemovalWeek, setting := 0]
  
  # from temp data
  # model #  hm = maturityDegreeDays / (avtemp9 - baseTemp)
  dtw[, hmFromTemp := predict(modelsGiven$hmFromTemp
                              , newdata = data.table(avtemp9 = avtemp9))]
  
  dtw[, shiftedHM := shift(hmFromTemp
                           , hmFromTemp[firstNonNA(hmFromTemp) + sf]
                           , type = "lead")[1:.N]]
  
  dtw[, harvest := harvestComingUp(setting = setting, hm = shiftedHM)[1:.N]]
  
  dtw[, afw     := frollmeanMirror(afw, n = afwSmoothing)]
  
  dtw[, yieldHM := harvest * afw / 1000]
  dtw[, yieldLZ := lzman(yield)]
  
  return(dtw)
}




extrapolateYieldSummary1 <- function(
  dtw
  , modelsGiven = list()
  , daysPerRow = 7
  , enrichPars 
  , ...
){
  # drivers of temp
  dtw[, light.sum.total.day := predict(modelsGiven$radModel
                                       , newdata = data.table(weekno = weekno))]
  dtw[, OUTEMP := predict(modelsGiven$OUTEMPModel
                          , newdata = data.table(weekno = weekno))]
  # temp
  dtw[, temp24hr := predict(modelsGiven$tempModel
                            , newdata = data.table(light.sum.total.day = light.sum.total.day
                                                   , OUTEMP = OUTEMP)  )]
  dtw[, avtemp3 := frollmeanMirror(temp24hr, n = 3)]
  dtw[, avtemp9 := frollmeanMirror(temp24hr, n = 9)]
  
  # setting  
  dtw[, trussSpeed := predict(modelsGiven$temp2truss
                              , newdata = data.table(avtemp3 = avtemp3))]
  dtw[, setting := trussSpeed * pruning * stem.density.setting]
  dtw[, setting := setting * (1 - fruitFall*(.25 + weekno/10))]
  dtw[week(dateTime) > headRemovalWeek, setting := 0]
  
  if (!predictAllFromTemp){
    # harvest, PL and HM
    dtw[, harvest := yield * 1000 / afw]
    
    dtw[, setting.cum := aphCumsum(setting) + initHanging]
    dtw[, harvest.cum := aphCumsum(harvest)]
    dtw[, plantLoad := setting.cum - harvest.cum]
    
    # data: setting and harvest
    dtw[, harvestMaturity := calcMaturity(setting = setting.cum
                                          , harvest = harvest.cum)]
    
    dtw[, harvestMaturity := predict(lm(
      harvestMaturity ~ avtemp9 + temp24hr + avtemp3 
      , na.action = na.exclude)
      , newdata = .SD)]
    
    dtw[, speedup := -diff0(harvestMaturity)]
    
    dtw[, shiftedHM := shift(harvestMaturity
                             , first(hmFromTemp) + sf
                             , type = "lead")[1:.N]]
  } else {
    # data: temp only
    dtw[, hmFromTemp := predictMaturity(temperature = daysPerRow*temp24hr
                                        , maturityDegreeDays = maturityDegreeDays
                                        , baseTemp = baseTemp, n = 1)]
    # model #  hm = maturityDegreeDays / (avtemp9 - baseTemp)
    dtw[, hmFromTemp := predict(modelsGiven$hmFromTemp, newdata = .SD)]
    
    dtw[, shiftedHM := shift(hmFromTemp
                             , first(hmFromTemp) + sf
                             , type = "lead")[1:.N]]
  }
  
  dtw[, harvest := harvestComingUp(setting = setting, hm = shiftedHM)[1:.N]]
  dtw[, afw     := frollmeanMirror(afw, n = afwSmoothing)]
  dtw[, yieldHM := harvest * afw / 1000]
  dtw[, yieldLZ := lzman(yield)]
  return(dtw)
}
