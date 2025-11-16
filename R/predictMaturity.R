#' predictMaturity
#' 
#' @param temperature numeric vector
#' @param maturityDegreeDays numeric vector
#' @examples \dontrun{
#'   # 210 days of 20 oC
#'   temperature <- rep(20, 7*30)
#'   maturityDegreeDays <- c(rep(1200, 7*20), rep(1000, 7*10))
#'   (hm <- predictMaturity(temperature, maturityDegreeDays, deltaTime = 1))
#'   plot(hm)
#'   
#'   # 30 weeks of 20 oC
#'   temperature <- rep(7*20, 30)
#'   maturityDegreeDays <- c(rep(1200, 20), rep(1000, 10))
#'   (hm <- predictMaturity(temperature, maturityDegreeDays))
#'   plot(hm)
#'   
#'   # rising temperature
#'   temperature <- c(rep(20, 7*20), rep(30, 7*10))
#'   maturityDegreeDays <- 1200
#'   (hm <- predictMaturity(temperature, maturityDegreeDays, deltaTime = 1))
#'   plot(hm)
#'   
#'   temperature <- c(rep(7*20, 20), rep(7*30, 10))
#'   maturityDegreeDays <- 1200
#'   (hm <- predictMaturity(temperature, maturityDegreeDays))
#'   plot(hm)
#'   
#'   temperature <- seq(7*20, 7*30, 2)
#'   maturityDegreeDays <- 1200
#'   (hm <- predictMaturity(temperature, maturityDegreeDays, baseTemp = 12))
#'   plot(hm)
#'   
#'   # negative tempSum is prevented
#'   predictMaturity(temperature = rep(20, 5), 1200, baseTemp = 0)
#'   predictMaturity(temperature = rep(20, 5), 1200, baseTemp = 10)
#'   
#'   (hm <- predictMaturity(rep(7*20, 5)))
#'   plot(hm)
#' }
#' @importFrom zoo na.approx
#' @export
predictMaturity <- function(temperature = rep(deltaTime*20, 25)
                          , maturityDegreeDays = 1200
                          , baseTemp = 0
                          , deltaTime = 7
                          , n = 1) {
  temperature.cu <- hcumsum(pmax(0, temperature - deltaTime * baseTemp), start0 = FALSE)
  temperature.cu <- frollmeanMirror(temperature.cu, n = n, align = "right")
  indexOfSettingR <- findIntervalReal(temperature.cu - maturityDegreeDays, temperature.cu)
  index <- seq(length(temperature.cu))
  harvest.maturity <- index - indexOfSettingR
  return(harvest.maturity)
}
