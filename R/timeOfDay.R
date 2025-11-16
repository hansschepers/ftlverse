#' timeOfDay
#' 
#' @export
timeOfDay <- function(d = Sys.time(), start = hfloor_date(Sys.time()) ){
  # d_li <- unclass(as.POSIXlt(d))
  # d_li[c("wday")] <- NA
  # d_li[c("mday", "mon", "yday")] <- 1
  # d_li[c("year")] <- year.oi
  # tod <- as.POSIXct(structure(d_li, class = c("POSIXlt", "POSIXt"), tz = d_tz))
  
  tod <- start + (d - hfloor_date(d))
  tod
  return(tod)
}
