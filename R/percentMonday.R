#' percentMonday
#' 
#' @importFrom data.table wday
#' @export 
percentMonday <- function(x, weekDayNumber = 2){
  ww <- data.table::wday(x)
  perc <- 100 * sum(weekDayNumber == ww, na.rm = TRUE) / length(ww)
  return(perc)
}
