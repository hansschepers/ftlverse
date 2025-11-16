#' envelopeSmooth
#' 
#' @export
envelopeSmooth <- function(dtmp
                           , xoi = "day_of_year"
                           , span = 7
                           , degree = 7
                           , finalSmoothSpan = 1
                           , bycols = character()
                           , padType = "circular"
                           , yoisNoSmoothing = c("stemDensity.setting", "pruning")
){
  if ("processName" %in% names(dtmp)){
    bycols <- union("processName", bycols)
  }
  dtmp <- copy(dtmp)
  aphKey(dtmp)
  
  dtmp[, hmax := padProcess(hmax
                            , fun = "frollapply", npad = 3, padType = padType
                            , funargs = list(align = "center"
                                             , FUN = "hmax"
                                             , n = span))
       , by = bycols]
  
  dtmp[, hmin := padProcess(hmin
                            , fun = "frollapply", npad = 3, padType = padType
                            , funargs = list(align = "center"
                                             , FUN = "hmin"
                                             , n = span))
       , by = bycols]
  
  dtmp[, hmean := padProcess(hmean
                             , fun = "frollapply", npad = 3, padType = padType
                             , funargs = list(align = "center"
                                              , FUN = "hmean"
                                              , n = span))
       , by = bycols]
  
  dtmp[, hmax := aphApprox2(hmax), by = bycols]
  dtmp[, hmin := aphApprox2(hmin), by = bycols]
  dtmp[, hmean := aphApprox2(hmean), by = bycols]
  
  if (degree > 0){
    dtmp[!processName %in% yoisNoSmoothing
         , hmax2 := polySmooth(y = hmax, x = get(xoi), degree = degree), by = bycols]
    dtmp[!processName %in% yoisNoSmoothing
         , hmin2 := polySmooth(y = hmin, x = get(xoi), degree = degree), by = bycols]
    dtmp[!processName %in% yoisNoSmoothing
         , hmean2 := polySmooth(y = hmean, x = get(xoi), degree = degree), by = bycols]
  }
  
  # pggs(aphMelt(dtmp), foi = "kpi")
  if (finalSmoothSpan > 1){
    dtmp[!processName %in% yoisNoSmoothing
         , hmax := frollmeanMirror(hmax, n = finalSmoothSpan, padType = padType), by = bycols]
    dtmp[!processName %in% yoisNoSmoothing
         , hmin := frollmeanMirror(hmin, n = finalSmoothSpan, padType = padType), by = bycols]
    dtmp[!processName %in% yoisNoSmoothing
         , hmean := frollmeanMirror(hmean, n = finalSmoothSpan, padType = padType), by = bycols]
  }
  dtmp[]
}
