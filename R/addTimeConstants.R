#' addTimeConstants
#' @examples \dontrun{
#'   DT <- data.table::data.table(dateTime = ISOdatetime(2021, 10, 28, 2, 3, 4))
#'   DT <- data.table::data.table(dateTime = ISOdatetime(2021, 10, 24, 2, 3, 4))
#'   addTimeConstants(DT)
#' }
#' @importFrom logger log_trace
#' @export
addTimeConstants <- function(DT
                             , timeCol = "dateTime"
                             , periods = c("yr", "qrt", "mon", "wk"
                                           , "wDate", "dDate", "doy", "hr")
                             , week_start = 2
                             ){
  data.table::setDT(DT)
  DT <- data.table::copy(DT)
  if (!timeCol %in% names(DT)) {
    stop("timeCol must be a col in the supplied data.table")
  }
  for (fun in periods) {
    logger::log_trace("fun: {fun}")
    res <- switch(fun
    , yr    = lubridate::year(DT$dateTime)
    , qrt   = lubridate::quarter(DT$dateTime)
    , mon   = lubridate::month(DT$dateTime)
    , wk    = lubridate::isoweek(DT$dateTime)
    , wDate = lubridate::floor_date(DT$dateTime, unit = "weeks", week_start = week_start)
    , dDate = lubridate::floor_date(DT$dateTime, unit = "days")
    , doy   = lubridate::yday(DT$dateTime)
    , hr    = lubridate::hour(DT$dateTime)
    )
    DT[, (fun) := res]
  }
  DT[]
}

