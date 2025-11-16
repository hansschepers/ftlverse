#' aggCycle2week
#' 
#' @export
aggCycle2week <- function(dtf
                          , pns = c("RAD")
                          , pnrn = c(RAD = "light.sum.total.day")
                          , yoisWeek = aphConstants()$yoisWeek
){
  
  dtf <- dtf[!(processName == "RAD" & value > 1000)]
  
  J2W <- universalConstants()$J2W
  dtf[, wk := week(dateTime)]
  
  newdtwks <- list()
  ii <- pns[1]
  for (ii in pns){
    dtwk <- dtf[processName == pns[i]
                , .(dateTime = hmean(dateTime)
                    , value = J2W*hmean(value), N = .N)
                , by = c(voi, "wk", foip, foic)]
    dtwk$processName <- pn[ii]
    hour(dtwk$dateTime) <- 12
    dtwk[, dateTime := hfloor_date(dateTime)]
    dtwk[, N := NULL]
    newdtwks[[ii]] <- dtwk
  }
  
  dtf <- dtf[processName %in% yoisWeek]
  hour(dtf$dateTime) <- 12
  dtf[, dateTime := hfloor_date(dateTime)]
  
  dtf2 <- rbindlist(c(list(dtf = dtf), newdtwks), fill = TRUE)
  aphKeyp(dtf2)
  dtf2
}
