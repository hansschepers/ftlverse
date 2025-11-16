#' getEnrichPars
#' @examples \dontrun{
#'   df <- cbind(large = getEnrichPars(), cherry = getEnrichPars("Cherry"))
#'   as.data.table(df, keep.rownames = TRUE)
#' }
#' @export
getEnrichPars <- function(segment = "large"
                          , variety = "merlice"          # ignored
                          , cycle = c("unlit", "lit")[1] # ignored
){
  pars <- switch(tolower(segment)
         , cherry = list(tempSpan = 12
                         , temperatureShift = 0
                         , trussTempSpan = 3
                         , temp24hr.constant = 20
                         # truss
                         , trussMix = 0.75   # ok
                         , fldMix = 0   # ok
                         , sf = 1         # ok
                         , harvestMaturityShiftedfromActual = -1 # 0  # ok
                         
                         , initHanging = 0
                         , singleFruitFallWeek = 0
                         , singleFruitFallNumber = 1
                         
                         , afwSmoothing = 1
                         , fruitFall = 0
                         , fruitFallExpo = 0
                         , maxSetting = 100
                         , radScenarioMultiplier = 1
                         , tweak.temp24hr = 0
                         )
         # default (large)
         , list(tempSpan = 12
                , temperatureShift = 0
                , trussTempSpan = 3
                , temp24hr.constant = 20
                # truss
                , trussMix = 0.75   # ok
                , fldMix = 0        # ok
                , sf = -1           #TODO
                , harvestMaturityShiftedfromActual = -1 # 0  # ok
                
                , initHanging = 0
                , singleFruitFallWeek = 0
                , singleFruitFallNumber = 1
                
                , afwSmoothing = 1
                , fruitFall = 0
                , fruitFallExpo = 0
                , maxSetting = 40
                , radScenarioMultiplier = 1
                , tweak.temp24hr = 0
                )
  )
  
  pars <- mergeParameters(pars, getCropPars(segment = segment))
  pars <- mergeParameters(pars, getSteeringPars(segment = segment))
  pars
}
