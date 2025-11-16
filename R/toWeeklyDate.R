#' toWeeklyDate
#'
#' returns the date of the last Monday before x
#'
#' @examples \dontrun{
#'   # today and next days
#'   dateTimes <- seq(c(Sys.Date()), by = "day", length.out = 15)
#'   library(lubridate)
#'   wday(dateTimes)  # 1 = Sunday
#'   toWeeklyDate(dateTimes)
#'
#'   # possibly tricky when using weeknumber based method: across newyear...
#'   dateTimes <- as.Date(18610 + 0:20, "1970-01-01")  # this is 2020-12-14, a Monday
#'   str(dateTimes)
#'   sapply(1:7, function(x) toWeeklyDate(as.POSIXct(dateTimes), x), simplify = F)
#'   # all wrong:
#'   sapply(1:7, function(x) toWeeklyDate(dateTimes, x, year = 2020))
#'   lapply(1:7, function(x) toWeeklyDate(dateTimes, x, year = 2020))
#'   sapply(setNames(2020:2030, 2020:2030), function(x) toWeeklyDate(dateTimes, 2, x))
#'   lapply(setNames(2020:2030, 2020:2030), function(x) toWeeklyDate(dateTimes, 2, x))
#' }
# @importFrom data.table week
#' @importFrom lubridate  days wday
#' @export
toWeeklyDate <- function(dateTimes
                         , weekStart = 2       # Monday
                         , year = NULL
                         , tz.oi = lubridate::tz(dateTimes)
){
  if (is.character(dateTimes)){
    dateTimes <- ymd(dateTimes)
  }
  # <- <- <- <- wasPOSIX <- is.POSIXt(dateTimes)
  # dateTimes <- as.Date(dateTimes)
  # if (weekStart < 0){
  #   # old, strange, risky, unitelligible
  #   dateTimes <- as.Date(paste(data.table:::year(dateTimes), data.table:::week(dateTimes), abs(weekStart)), format="%Y %U %u")
  #   # as.Date(paste(data.table:::year(dateTimes), lubridate:::week(dateTimes), abs(weekStart)), format="%Y %U %u")
  # } else {
  weekDays <- lubridate::wday(dateTimes, week_start = 7)
  dateTimes <- dateTimes - lubridate::days((weekDays - weekStart + 7) %% 7)
  lubridate::tz(dateTimes) <- tz.oi
  # }
  if(is.numeric(year)){
    dateTimes <- syncYear(dateTimes, yearSync = year)
    lubridate::tz(dateTimes) <- tz.oi
  }
  dateTimes
}

#' toMonday
#'
#' @export
toMonday <- toWeeklyDate

