#' makeEventlist
#' @examples \dontrun{
#'   x <- rep(7, 11)
#'   x
#'   makeEventlist(x)
#'   x <- c(rep(7, 3), 6, rep(5, 2), rep(1, 2))
#'   x
#'   ev <- makeEventlist(x, parName = "pruning")
#'   ev
#'   dd <- eventDrivers(drivers = list(Time = 7*(0:50)), eventList = ev, nTime = 51, iseed = NULL)
#'   str(dd)
#'   dd
#' }
#' @export
makeEventlist <- function(x
                          , parName = "parameterX"
                          , sh = 0){
  WAP <- c(0)
  value <- x[1]
  ind <- 0
  while(!is.na(ind)){
    ind <- which(x != x[1])[1]
    if (is.na(ind)) break
    WAP <- c(WAP, ind)
    value <- c(value, x[ind])
    log_debug("parName, WAP, value {paste(parName, ind, x[ind])}")
    x <- x[ind:length(x)]
  }
  WAP[-(1:2)] <- WAP[-(1:2)] - 1
  eventList <- list(x = list(WAP = cumsum(WAP)-sh
                             , value = value))
  # names(eventList)
  eventList <- setNames(eventList, parName)
  eventList
}
