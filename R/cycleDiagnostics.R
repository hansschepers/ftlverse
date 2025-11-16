#' cycleDiagnostics
#' 
#' @export
cycleDiagnostics <- function(DT
                             , thr = 100
                             , voi = "processName"
                             , foi = "cycle_syn"
                             , doplot = FALSE
){
  DTfocused <- cycleFocus(DT)
  DTdiagn <- DTfocused[, hsummary(dateTime, hfuns = "quick")
                       , by = c(voi, foi)]
  DTdiagn[, lengthClass := ifelse(hlength > thr, "IoT", "Weekdata")]
  setkeyv(DTdiagn, c("lengthClass", "hlength"))
  DTdiagn[, processName := fixFactor(processName)]
  if (doplot){
    ppL <- pggs(DTdiagn
                , xoi = "hlength", yoi = voi
                , foi = intersect(c(foi, "lengthClass"), names(DTdiagn))[1]
                , pointAlpha = .3
                , facet_w = "lengthClass"
                # , legend = "none"
                , free_x = TRUE
                , geom = "point", psize = 4, fsize = 14, ysize = 8)
  }
  DTdiagn[]
}
