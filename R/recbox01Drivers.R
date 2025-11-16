#' recbox01Drivers
#' @examples \dontrun{
#'   driversList <- recbox01Drivers(parms = recbox01Parms())
#'   all_drivers2funs(driversList)
#' }
#' 
#' @export
recbox01Drivers <- function(times = seq(0, 5*parms$TT, length.out = 1001)
                            , parms = recbox01Parms()
                            , doplot = FALSE){
  neededParmNames <- c("TT" , "beta", "gc", "g0")
  
  ################################ time only ! #################################
  dd <- data.table(time = times)
  dd[, gt := blockPulse(time, pp = parms[neededParmNames])]
  dd
  if (doplot){
    p_dT <- ppggs(aphMelt(dd, id.vars = "time")
                  , title = list2title(parms[neededParmNames]))
    print(p_dT)
  }
  driversList <- list(time = dd[])
  .driversList <<- driversList
  driversList
}

