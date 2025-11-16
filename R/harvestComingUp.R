#' fracShift1
#' 
# @export
fracShift1 <- function(x, n, i, m = 4 + length(x) + ceiling(hmax(n))) {
  # log_trace("i {i}")
  # log_trace("m {m}")
  res <- c(rep(0, i-1), replaceNa(fracShift(x[i],  n[i]), 0))
  # log_trace("res {length(res)}")
  c(res, rep(0, max(0, m - length(res))))
}

#' hshift
#' 
#' @export
hshift <- fracShift

#' harvestComingUp
#' 
#' shifts setting by a variable hm (harvestMaturity)
#' 
#' @export
harvestComingUp <- function(setting
                            , hm
                            , HCUnoise = 0
                            , HCUzoom = 7
                            , npad = 1
                            , hmShiftStep = 0
                            , centered = TRUE){
  
  if (length(setting) > length(hm)){
    message("trimming setting to length of hm...")
    setting <- setting[seq_along(hm)]
  }
  if (length(hm) > length(setting)){
    message("HCU: first trimming hm to length of setting")
    hm <- hm[seq_along(setting)]
  }
  if (length(setting) == 1){
    HCUnoise <- 0
  }
  
  if (npad > 1){
    setting <- hfrollmean(setting, n = npad, align = "center")
    hm <- hfrollmean(hm, n = npad, align = "center")
  }
  if (hmShiftStep > 0){
    hm <- roundCentered(hm, shiftStep = hmShiftStep, centered = centered)
  }
  
  if (HCUzoom > 1){
    setting <- timeZoom(setting, HCUzoom, scaleDown = HCUzoom)
    hm      <- timeZoom(hm, HCUzoom, scaleDown = 1/HCUzoom)
  }
  # setting <- hfilter(setting)
  # hm <- hfilter(hm)
  
  if(HCUnoise > 0.001){
    hcu1 <- sapply(seq_along(setting)
                   , function(i) fracShift1(x = setting/3, n = hm + HCUnoise, i))
    hcu2 <- sapply(seq_along(setting)
                   , function(i) fracShift1(x = setting/3, n = hm - HCUnoise, i))
    hcu3 <- sapply(seq_along(setting)
                   , function(i) fracShift1(x = setting/3, n = hm, i))
    ncols <- min(ncol(hcu1), ncol(hcu2), ncol(hcu3))
    hcu <- hcu1[seq_len(ncols),] + hcu2[seq_len(ncols),] + hcu3[seq_len(ncols),]
  } else {
    if(HCUnoise < -0.001){
      hcu1 <- sapply(seq_along(setting)
                     , function(i) fracShift1(x = setting/2, n = hm + HCUnoise, i))
      hcu2 <- sapply(seq_along(setting)
                     , function(i) fracShift1(x = setting/2, n = hm - HCUnoise, i))
      ncols <- min(ncol(hcu1), ncol(hcu2))
      hcu <- hcu1[seq_len(ncols),] + hcu2[seq_len(ncols),]
    } else {
      
      # default here ===========================================================
      hcu <- sapply(seq_along(setting)
                    , function(i) fracShift1(x = setting, n = hm, i))
    }
  }
  # .hcu <<- hcu
  # hcu <- .hcu
  harvest <- apply(hcu, 1, sum)
  harvest <- harvest[seq_along(setting)]
  
  if (HCUzoom > 1){
    # smooth
    harvest <- hfrollmean(harvest, HCUzoom, align = "center")
    # aggregate (zoom out):
    harvest <- apply(matrix(data = harvest
                            , ncol = HCUzoom
                            , byrow = TRUE), 1, sum)
  }
  
  # harvest <- harvest[seq(lastNonNA(harvest, suspect = 0) )]
  # # harvest <- harvest[seq_along(setting)]
  # harvest <- procrustes(harvest
  #                   , bed = setting
  #                   , pos = c("left", "right")[2] #, "NA", "both"
  #                   , repl = c(NA, 0)[1]
  # )
  
  harvest
}


#' harvestComingUpStSt
#' assumes the first setting has been going on before the time series started.
#' 
#' @examples \dontrun{
#'   harvestComingUp(rep(15, 7), rep(3, 7))
#'   harvestComingUp(rep(15, 7), rep(3, 7), HCUzoom = 1)
#'   
#'   harvestComingUpStSt(rep(15, 7), rep(3, 7))
#'   harvestComingUpStSt(rep(15, 7), rep(3, 7), HCUzoom = 1)
#' }
#' @export
harvestComingUpStSt <- function(setting, hm, ...){
  addn <- ceiling(hmax(hm))
  res <- harvestComingUp(c(rep(setting[1], addn), setting)
                         , c(rep(hm[1], addn), hm), ...)
  res <- res[(addn+1):length(res)]
  res
}
