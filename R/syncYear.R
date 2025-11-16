#' syncYear
#' @examples \dontrun{
#'   dateTimes <- seq.Date(as.Date("2019-04-01"), by = "year", length.out = 6)
#'   syncYear(dateTimes, 2000)
#' }
#' @importFrom lubridate year years
#' @export
syncYear <- function(dateTimes
                     , yearSync = lubridate::year(Sys.Date())
){
  dateTimes - lubridate::years(lubridate::year(dateTimes) - as.numeric(yearSync))
}


#' syncDay
#' @examples \dontrun{
#'   dateTimes <- Sys.time() + lubridate::days(1:6)
#'   # dateTimes <- seq.Date(as.Date("2019-04-01"), by = "day", length.out = 6)
#'   syncDay(dateTimes, 2000, 1, 1)
#' }
#' @importFrom lubridate year years
#' @export
syncDay <- function(dateTimes
                    , yearSync = lubridate::year(Sys.Date())
                    , monSync = lubridate::month(Sys.Date())
                    , daySync = lubridate::yday(Sys.Date())
){
  dateTimes - lubridate::years(lubridate::year(dateTimes) - as.numeric(yearSync))
  # - lubridate::years(lubridate::month(dateTimes) - as.numeric(monSync))
  # - lubridate::years(lubridate::yday(dateTimes) - as.numeric(daySync))
}
