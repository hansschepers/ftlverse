#' excelDateToDate
#' @examples \dontrun{
#'   excelDateToDate(40729) # "2011-07-05"
#' }
#' @export
excelDateToDate <- function(x) {
  as.Date(x-2, "1900-01-01")
}

#' excelDateToTime
#' @examples \dontrun{
#'   excelDateToTime(40729.33333333333333333)
#' }
#' @export
excelDateToTime <- function(x) {
  x <- x * 24*3600 
  as.POSIXct(x - 2*24*3600, tz = "UTC", origin = "1900-01-01")
}

