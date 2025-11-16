#' as_date_TZ
#'
#' alternative to `as.Date()` that keeps the timezone and is not as slow as
#' `lubridate::floor_date()`. If timezone 
#'
#' @param timeObject vector of date or dateTimes
#' @param tz char tz
#'
#' @return converted timeObject
#' @importFrom lubridate tz force_tz
#' @export
as_date_TZ <- function(timeObject, tz = lubridate::tz(timeObject)) {
  newDate <- lubridate::force_tz(timeObject, "UTC")
  newDate <- as.Date(newDate)
  lubridate::force_tz(newDate, tz)
}
