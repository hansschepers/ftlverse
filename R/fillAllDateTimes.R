#' fillAllDateTimes
#' 
#' @export
fillAllDateTimes <- function(dtw1
                             , timeStep = "1 hour"
                             , bycols = aphFactors(dtw1)
                             , yois = aphVariableLevels(dtw1)
){
  (origTZ <- tz(dtw1$dateTime))
  dtw1[, dateTime := force_tz(dateTime, "UTC")]
  splitList <- split(dtw1, by = bycols)
  # dtt <- splitList[[1]]
  dtall <- lapply(splitList, \(dtt){
    newdates <- seq.POSIXt(hmin(dtt$dateTime), hmax(dtt$dateTime), by = timeStep, tz = "UTC")
    fullDateTime <- data.table(dateTime = newdates)
    fullDateTime <- cbind(fullDateTime, dtt[1, ..bycols])
    fullDateTime
    print(nrow(fullDateTime))
    print(dim(dtt))
    merge(fullDateTime, dtt, all = TRUE)
  })
  dtw2 <- rbindlist(dtall)
  
  # sapply(dtw2, sumna)
  setnafill(dtw2, type="locf", cols=yois)
  # sapply(dtw2, sumna)
  tz(dtw2$dateTime)
  dtw2[, dateTime := with_tz(dateTime, origTZ)]
  dtw2[]
}


if(F){
  ddoi <- "2022-03-27"
  yday(ddoi)
  # ddoi <- "2022-01-17"
  ww <- seq.POSIXt(ISOdatetime(2022, 3, 27, 0, 0, 0, tz = "UTC")
             , ISOdatetime(2022, 3, 28, 0, 0, 0, tz = "UTC"), by = "1 hour", tz = "UTC")
  tz(ww)
  force_tz(ww, "europe/Amsterdam")
  with_tz(ww, "europe/Amsterdam")
}
