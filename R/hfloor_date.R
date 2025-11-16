#' hfloor_date
#' @examples \dontrun{
#'   ss <- Sys.time()
#'   lubridate::floor_date(ss, unit = "days")
#'   hfloor_date(ss)
#'   x1 <- hfloor_date(ss + lubridate::days(0:8))
#'   x2 <- lubridate::floor_date(ss + lubridate::days(0:8), unit = "days")
#'   all.equal(x1, x2)
#'   ss2 <- ss + lubridate::days(0:10)
#'   #library(microbenchmark)
#'   res <- microbenchmark(hfloor_date(ss2)
#'    , hfloor_date2(ss2)
#'    , lubridate::floor_date(ss2, unit = "days")
#'    , round(ss2, units = "days")
#'    , times=1000L)
#'    print(res)
#'    boxplot(res)
#' }
#' @export
hfloor_date <- function(d, ...){
  # d_tz <- tz(d)
  # d_cl <- class(d)
  d_li <- unclass(as.POSIXlt(d))
  # if (d_tz[1] == "") d_tz <- "UTC"
  d_tz <- attr(d_li, "tzone")
  # d_zone <- d_li$zone
  d_li[c("sec", "min", "hour")] <- 0
  d_lt <- structure(d_li, class = c("POSIXlt", "POSIXt"), tz = d_tz)
  return(as.POSIXct(d_lt))
}

#' hfloor_date2
#' 
#' @export
hfloor_date2 <- function(d, d_tz = "CET"){
  d_li <- unclass(as.POSIXlt(d))
  d_li[c("sec", "min", "hour")] <- 0
  return((as.POSIXct(structure(d_li, class = c("POSIXlt", "POSIXt"), tz = d_tz))))
}
