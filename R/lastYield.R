#' lastYield2
#' @export
lastYield2 <- function(DT
                      , yois = c("yield", "stem.diam", "plantload.fruits.m2"
                                 , "temp24hr", "light.sum.total.day", "lightsum.day.years.out"
                                 , "trussSpeed", "yield.cum"
                                 , "afw", "setting", "harvest")
                      , n = 6
){
  setDT(DT)
  fois <- aphFactors(DT)
  if ("resetDay" %in% names(DT)){
    fois <- c(fois[1], "resetDay", fois[-1])
  }
  fois
  DTw <- hdcast(DT[processName %in% yois])
  
  if ("wk" %in% names(DTw)){
    DTw[, wk := lubridate::isoweek(dateTime)]
    print(tail(DTw, n))
    DTw[, wk := NULL]
  } else {
    print(tail(DTw, n))
  }
  DTw[]
}
