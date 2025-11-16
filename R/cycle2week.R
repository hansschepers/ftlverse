#' cycle2week
#' 
#' @export
cycle2week <- function(DT = NULL
                       , thr = 100
                       , DTfocused = NULL
                       , DTdiagn = NULL
                       ){
  if (is.null(DTfocused)) {
    DTfocused <- cycleFocus(DT)
  }
  if (is.null(DTdiagn)) {
    DTdiagn <- cycleDiagnostics(DTfocused)
  }
  weekProcessNames <- unique(DTdiagn[hlength < thr, processName])
  DTwk <- DTfocused[processName %in% weekProcessNames]
  hour(DTwk$dateTime) <- 12
  DTwk[, dateTime := as.Date(dateTime)]
  DTwk[, processName := as.character(processName)]
  aphKey(DTwk)
  DTwk
}
