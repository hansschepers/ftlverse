#' afwPredictionOLD
#' Prediction of Average Fruit Weight as driven by temperature (and time)
#' 
#' @examples \dontrun{
#'   afwPredictionOLD()
#'   afwPredictionOLD(temp = 15)
#'   afwPredictionOLD(temp = seq(15, 25, 1))
#'   afwPredictionOLD(temp = seq(15, 25, 1), maturityDegreeDays = 800)
#'   afwPredictionOLD(temp = seq(15, 25, 1), maturityDegreeDays = 1200)
#'   # stable AFW:
#'   (the swelling and the coloring are both increased by Temperature by the same amount)
#'   afwPredictionOLD(temp = seq(15, 25, 1), rel_swelling.rate_max = 0.075)
#'   afwPredictionOLD(temp = seq(15, 25, 1), baseTemp = 6, rel_swelling.rate_max = (20-6)/200)
#'   afwPredictionOLD(temp = seq(15, 25, 1), maturityDegreeDays = 1200, rel_swelling.rate_max = (20-5)/200)
#'   afwPredictionOLD(temp = seq(15, 25, 1), maturityDegreeDays = 850, rel_swelling.rate_max = (20-5)/200)
#'   afwPredictionOLD(temp = seq(15, 25, 1), maturityDegreeDays = 850, rel_swelling.rate_max = (20-6)/200)
#'   afwPredictionOLD(temp = seq(15, 25, 1), maturityDegreeDays = 850, rel_swelling.rate_max = (20-6)/200, initialFruitsize = 2)
#'   afwPredictionOLD(temp = seq(15, 25, 1), maturityDegreeDays = 850, rel_swelling.rate_max = (20-6)/200, initialFruitsize = 6)
#'   afwPredictionOLD(temp = seq(15, 25, 1), baseTemp = 5, maturityDegreeDays = 1200, initialFruitsize = 2, rel_swelling.rate_max = (20-5)/200)
#'   afwPredictionOLD(temp = seq(15, 25, 1), baseTemp = 6, maturityDegreeDays = 850, initialFruitsize = 12, rel_swelling.rate_max = (20-6)/200)
#'   
#'   afwPredictionOLD(temp = c(15, 25, 30), hm = c(80, 60, 50))
#' }
#' @param afwPars a list or numeric vector, with parameters (defaults given)
#' @export
afwPredictionOLD <- function(afwPars = list(
  # logistic growth parameters
  initialFruitsize = 2
  , fw_max = 180
  , rel_swelling.rate_max = 0.1
  # temperature, in celcius
  , temp = 20
  # coloring dependence on Temperature
  , maturityDegreeDays = 1000
  , baseTemp = 5
  # swelling dependence on Temperature
  , swelling_temp_ref = 20
  , swellingTempSensitivity = 0.005)
  , ...
){
  dots <- list(...)
  .dots <<- dots
  afwPars <- mergeParameters(afwPars, dots)
  
  with (afwPars, 
        {
          # harvest Maturity
          if (!"hm" %in% names(afwPars)){
            hm <- maturityDegreeDays / (temp - baseTemp)
          }
          # max relative swelling rate
          rr <- pmax( (temp - swelling_temp_ref) * swellingTempSensitivity + rel_swelling.rate_max, 0)
          # fruit size (logistic Growth)
          afw <- fw_max / (1 + (fw_max/initialFruitsize - 1) * exp(-rr * hm))
          
          list(temp = temp
               , rr = rr
               , hm = hm
               , afw = afw)
        })
}
