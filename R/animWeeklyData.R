#' animWeeklyData
#' 
#' @examples \dontrun{
#'   # see system.file("example/example_animWeeklyData.R", package = "ftlPlot")
#' }
#' @export
animWeeklyData <- function(DTwf
                           , maxTime = nrow(DTwf)
                           , positions = 15
                           , fruitFall = 0  # percentage e.g. 0.05
                           , maxSetting = 50
                           , internodeLength = 20 # cm
                           , baseHeight = 60    # cm
                           , verbosity = 0
                           , setting.name = "setting"
                           , harvest.name = "harvest"
){
  dtwf <- copy(DTwf)
  setnames(dtwf, setting.name, "setting")
  setnames(dtwf, harvest.name, "harvest")
  dtwf[, setting := setting * (1 - fruitFall)]
  dtwf[, setting := pmin(setting, maxSetting)]
  
  # dtwf <- dtwf[cumsum(setting) > 0]
  
  dtwf[, HA.cu := rev(hcumsum(rev(harvest), fillLeftNA = FALSE))]
  # print(dtwf)
  dtwf <- dtwf[rev(hcumsum(rev(harvest), fillLeftNA = FALSE)) > 0]
  # return(dtwf)
  
  
  dthang <- data.table(pos = 1:positions, FN = 0)
  # itime <- 8
  reslist <- list()
  itime.s <- seq(min(maxTime, nrow(dtwf)))
  itime <- 1
  for (itime in itime.s){
    dthang[, dateTime := dtwf[itime, dateTime]]
    
    dthang[, FN := shift(FN, 1)]
    dthang[1, FN := dtwf[itime, setting]]
    dthang[, FN.cu := rev(hcumsum(rev(FN), fillLeftNA = FALSE))]
    n2h <- dtwf[itime, harvest]
    log_trace("itime {itime}, n2h {n2h}, ")
    if (!is.na(n2h)){
      toHarv <- findIntervalReal(n2h, rev(dthang[, FN.cu])
                                 # , useDeprecatedVersion = TRUE
      )
      if (is.na(toHarv) | is.infinite(toHarv)) break
      from <- positions - floor(toHarv) + 1
      removedWholeTruss <- sum(dthang[from:positions, FN])
      log_trace("toHarv {toHarv}, from {from}, removed {removedWholeTruss}")
      
      dthang[from:positions, FN := 0]
      dthang[, removed := 0]
      dthang[from:positions, removed := FN]
      
      dthang[from-1, FN := FN - (n2h - removedWholeTruss)]
      
      dthang[from-1, removed := (n2h - removedWholeTruss)]
    }
    if (verbosity > 1) print(dthang[])
    reslist[[itime]] <- copy(dthang)
  }
  stemProfile <- rbindlist(reslist, fill = TRUE, idcol = "itime")
  
  stemProfile[, h := positions - pos + 1]
  stemProfile[, h := h*internodeLength + baseHeight]
  
  headHeight <- internodeLength * hmax(stemProfile[FN > 0, pos])# + baseHeight
  
  for (iiitime in seq(max(stemProfile$itime))){
    if (iiitime < 15 | iiitime > 25){
      cat("-")
      mip <- min(stemProfile[itime == iiitime & FN > 0, h])
      stemProfile[itime == iiitime, h := pmax(baseHeight, h - mip + baseHeight)]
    } else {
      cat("+")
      map <- max(stemProfile[itime == iiitime & FN > 0, h])
      stemProfile[itime == iiitime, h := h - map + headHeight]
    }
  }
  # stemProfile[FN>0]
  stemProfile[, FW := pos^3]
  stemProfile[]
}
