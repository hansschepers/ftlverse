#' dt2eventList
#' @examples \dontrun{
#'   planPath <- "C:/Users/hhsche2/OneDrive - Bayer/Personal Data/__APH2022/fv22w32planData.xlsx"
#'   obs <- as.data.table(readxl::read_xlsx(planPath, na = "NA"))
#'   dt2eventList(obs)
#' }
#' @export
dt2eventList <- function(obs
                       , segmentName = "large"
                       , yois2extract = c("stem.density.setting", "RTR", "pruning"
                                          , "ECIRRI", "GHCO2C", "LAI")
                       , sh = 0){
  if ("processName" %in% names(obs)){
    obs <- hdcast(obs)
  }
  ww <- lapply(yois2extract, \(yoi){
    makeEventlist(aphApprox2( obs[[yoi]] )
                  , parName = yoi
                  , sh = sh)
  })
  eventList <- sapply(ww, c)
  eventList
}

