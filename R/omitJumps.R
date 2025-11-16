#' omitJumps
#' @examples \dontrun{
#'   x <- c(0, 1, 2, 11, 6, 9, 2, NA, 2, NA, 11, 11, NA, 13)
#'   x <- omitJumps(x, 5, 5)
#'   x
#' }
#' @export
omitJumps <- function(x, thr = 5, thr2 = thr
                      , minLength = 2){
  diffBack <- diff0(x)
  diffFW <- c(diffBack[-1], 0)
  diffFW2 <- -rev(diff0(rev(x)))
  
  kickOut <- abs(diffBack) > thr
  kickOut2 <- abs(diffFW) > thr2
  kickOut[is.na(kickOut) & is.na(kickOut2)] <- TRUE
  kickOut2[is.na(kickOut) & is.na(kickOut2)] <- TRUE
  
  kickOut[is.na(kickOut)] <- FALSE
  kickOut + 0
  x[seq_along(x)[kickOut]] <- NaN
  x
  
  kickOut2[is.na(kickOut2)] <- FALSE
  kickOut2 + 0
  x[seq_along(x)[kickOut2]] <- NaN
  x
  
  if (minLength > 1){
    xrun <- cumsum(0+is.na(x))
    tt <- table(xrun)
    
    longEnoughRun <- xrun %in% names(tt[tt >= minLength])
    x[!longEnoughRun] <- NaN
  }
  
  x
} 

