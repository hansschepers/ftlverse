#' hdiffrange
#' 
#' @export
hdiffrange <- function(x, na.rm = TRUE, timeUnitLength = 3600*24, digits=1, ...) {
  x <- na.omit(x)
  if (!length(x) > 0) {
    return(NA_real_)
  }
  if (sum(c("POSIXct", "POSIXt") %in% class(x))) {
    hdiffrangeDates(x, timeUnitLength = timeUnitLength, digits=digits, ...)
  }
  diff(range(x, na.rm=na.rm))
}


#' hdiffrangeDates
#' @param timeUnitLength integer in seconds, e.g. 'days' = 3600*24
#' @param digits integer to pass on to \code{round()}
#' @export
hdiffrangeDates <- function(x, timeUnitLength = 3600*24, digits=1, ...) {
  x <- na.omit(x)
  if (!length(x) > 0) {
    return(NA_real_)
  }
  round((as.numeric(hmax(x)) - as.numeric(hmin(x)))/timeUnitLength
        , digits = digits)
}


#' hminDiff
#' 
#' @export
hminDiff <- function(x, na.rm = TRUE, ...) {
  x <- na.omit(x)
  if (!length(x) > 0) {
    return(NA)
  }
  min(diff(x, ...), na.rm=na.rm)
}



#' hmaxDiff
#' 
#' @export
hmaxDiff <- function(x, na.rm = TRUE, ...) {
  x <- na.omit(x)
  if (!length(x) > 0) {
    return(NA)
  }
  max(diff(x, ...), na.rm=na.rm)
}

#' dateQA
#' @export
dateQA <- function(x){
  # library(xts)
  # percentMonday(x)
  # hminDiff(x)
  # hmaxDiff(x)
  # xts::periodicity(x)
  # toWeeklyDate(x)
  hsummary(x, hfuns = c("min", "max", "periodicity", "hdiffrange"
                        , "percentMonday", "hminDiff", 'hmaxDiff'))
}
