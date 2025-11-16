#' processHourlyZeros
#' @export
processHourlyZeros <- function(dt0_h
                          , yois = c("ECIRRI", "ONBUTBR", "ONBUTMT")
                          , maxChange = setNames(c(.1, 20, 20), yois)
                          , lb = 0
                          , ub = Inf
){
  dt0_h <- copy(dt0_h)
  aphKey(dt0_h)
  dt0_h[processName %in% yois, hsummary(value)
        , by = .(processName, cropseason_id) ]
  
  dt0_h[processName %in% yois & value <= lb, value := NA]
  dt0_h[processName %in% yois & value >= ub, value := NA]
  
  dt0_h[processName %in% yois & abs(c(0, diff(value))) > maxChange[processName], value := NA]
  dt0_h[processName %in% yois & abs(c(diff(value), 0)) > maxChange[processName], value := NA]
  
  dt0_h[processName %in% yois & is.na(shift(value, 1)), value := NA]
  dt0_h[processName %in% yois & is.na(shift(value, -1)), value := NA]
  
  aphKey(dt0_h)
  dt0_h[processName %in% yois , value := aphApprox2(value)
        , by = .(cropseason_id, processName, date = hfloor_date(local_time)) ]
  dt0_h[, date := NULL]
  
  dt0_h[processName %in% yois, hsummary(value)
        , by = .(cropseason_id, processName) ]
  aphKey(dt0_h)
  dt0_h
}