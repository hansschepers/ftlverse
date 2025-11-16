#' aphTimes
#'
#' returns the column names that, based on name, are date/time columns.
#' the order given is not the order from the (optional) DT, but from the (default) argument 'dois'
#'
#' @examples \dontrun{
#'   # what you put in is what you get out
#'   all.equal(aphTimes(), eval(formals(aphTimes)$dois))
#' }
# @importFrom Hmisc capitalize
#' @export
aphTimes <- function(DT = NULL
                     , caseSensitive = TRUE
                     , addClasses = c("Date", "POSIXct", "POSIXlt")
                     , dois = c("year", "yr"
                                , "quarter"
                                , "mon", "month", "MAP"
                                , "week", "weeknr", "weekno", "WAP", "wss", "wsfh", "weeknoCycle", "woy", "wk"
                                , "xoi", "x", "Var1"
                                , "wday", "tow"
                                , "itime", "doy", "day_of_year", "date", "dDate", "yday", "mday", "DAP"
                                , "hrslot", "hourslot", "hr", "hour"
                                , "hod", "tod", "time", "Time", "timedate", "dateTime", "local_time"
                                , "gc")
                     , ignore = character(0)
                     , alwaysIgnored = c("resetDay")
                     , alwaysAtStart = character(0)
){
  if (!is.null(DT)) {
    setDT(DT)
    nms <- names(DT)
    if (caseSensitive){
      dois <- intersect(c(dois), nms)
    } else {
      dois <- dois[tolower(dois) %in% tolower(nms)]
      dois <- nms[tolower(nms) %in% tolower(dois)]
    }
    log_trace("aphTimes|dois: {paste(dois, collapse = ', ')}")
    addFields <- character(0)
    for (classOI in addClasses){
      addFields <- c(addFields, nms[sapply(DT, inherits, classOI)])
    }
    dois <- unique(c(base::setdiff(addFields, dois), dois))
    dois <- unique(c(alwaysAtStart, dois))
    dois <- base::setdiff(dois, c(alwaysIgnored, ignore))
  }
  dois
}
