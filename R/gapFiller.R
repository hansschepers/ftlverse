#' gapFiller
#' example example/out/gapFillerDemo.R
#' @export
gapFiller <- function(xg
                      , gapLengthThreshold = 23
                      , variable.name = "processName"
                      , bycols = character(0)
                      , doi = "dateTime"
                      , cleanup = TRUE
                      , removeIsGap = TRUE
                      , doplot = FALSE
){
  xg <- copy(xg)
  bycols <- setdiff(bycols, variable.name)
  xg[, isgap := 0+is.na(value)]
  xg[, .N, by = isgap]
  aphKey(xg)
  xg[, changeGap := c(0, diff(isgap)), by = c(bycols, variable.name)]
  xg[, gapstart := 0 + (changeGap == 1)]
  # xg[, gapend := 0 + (changeGap == -1)]
  xg[, gapstart := cumsum(gapstart), by = c(bycols, variable.name)]
  
  xg[, gapID := gapstart * isgap]
  xg <- xg[, .(gapLength = .N), by = gapID][xg, on = "gapID"]
  # xg[, .N, by = gapID]
  # xg[gapID == 0, gapLength := 0]
  
  xr <- copy(xg)
  xr[, valueCopy := value]
  if (!"doy" %in% names(xr)) xr[, doy := yday(get(doi))]
  if (!"hr"  %in% names(xr)) xr[, hr  := hour(get(doi))]
  # reserve small gaps for later
  xr[gapLength <= gapLengthThreshold, value := -99]
  
  setkeyv(xr, c(bycols, variable.name, "hr", "doy"))
  xr[, value := aphApprox2(value), by = c(bycols, variable.name, "hr")]
  
  # now focus on small gaps
  xr[gapLength <= gapLengthThreshold, value := valueCopy]
  setkeyv(xr, c(bycols, variable.name, "doy", "hr"))
  xr[, value := aphApprox2(value), by = c(bycols, variable.name)]
  if (doplot){
    p1 <- pggs(xr[isgap > 0]
               , p = pggs(xr[isgap == 0], lwd = .1, xoi = doi)
               , xoi = doi
               , foi = "gapID"
               , geom = "point", psize = 2
               , fsize = 16, lineColor = "red", lwd = 1.5, lineAlpha = .4)
    print(p1)
  }
  if (cleanup){
  tmps <- c("changeGap", "gapstart", "gapID", "gapLength", "valueCopy")
  xr[, (tmps) := NULL]
  }
  if (removeIsGap){
    xr[, isgap := NULL]
  }
  xr
  return(xr[])
}
