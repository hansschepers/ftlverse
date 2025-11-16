#' mcDrivers
#' 
#' @examples \dontrun{
#'   mcDrivers()
#'   mcDrivers(drivers = list(Time = 1:10)
#'       , Pars = list(RTR = 3, RTRVariation = 0, RTRBias = 1))
#'   mcDrivers(drivers = list(Time = 1:10)
#'             , Pars = list(RTR = .0035, RTRVariation = .1, RTRVariationFrequency = 3, RTRBias = .01
#'                          , M = 5, MBias = 1, MVariation = 2
#'                          , W = 5, WBias = 0, WVariation = 0
#'                          , XVariation = .2)
#'               , iseed = 1234
#'               )
#' }
#' @export
mcDrivers <- function(drivers = list(Time = 1:20)
                      , Pars = list(RTR = .0035, RTRVariation = .1, RTRVariationFrequency = 3, RTRBias = .01
                                    , M = 5, MBias = 1, MVariation = 2
                                    , XVariation = .2)
                      , nTime = length(drivers[[1]])
                      , iseed = NULL
){
  if (!is.null(iseed)){
    set.seed(iseed)
  }
  drivers <- as.list(drivers)
  nmsV <- grep("Variation$", names(Pars), value = TRUE)
  nms <- sub("Variation$", "", nmsV)
  if (!length(nms)){
    return(drivers)
  }
  
  log_info("Parameters with mc Variation: {nms}")
  ok <- nms %in% names(Pars)
  if (length(ok) == length(nms)){
    log_info("leaving out {nms[!ok]}")
  }
  nms <- nms[ok]
  np <- length(nms)
  Center <- Pars[nms]
  Variation <- Pars[paste0(nms, "Variation")]
  
  Bias = rep(0, np)
  ok <- paste0(nms, "Bias") %in% names(Pars)
  Bias[ok] <- Pars[paste0(nms[ok], "Bias")]
  Bias
  
  Freq = rep(1, np)
  ok <- paste0(nms, "VariationFrequency") %in% names(Pars)
  Freq[ok] <- Pars[paste0(nms[ok], "VariationFrequency")]
  Freq
  
  ii <- 1
  for (ii in seq_along(nms)){
    nm <- nms[ii]
    if (any(Bias[[ii]] > 0, Variation[[ii]] > 0)){
      drivers[[nm]] <- Center[[ii]] + Bias[[ii]]
      drivers[[nm]] <- drivers[[nm]] + 
        rep(
          runif(ceiling(nTime/Freq[[ii]]) + 1
                , min = -Variation[[ii]]/2, max = Variation[[ii]]/2)
          , each = Freq[[ii]])[seq(nTime)]
    } else {
      log_info("no Bias nor Variation for {nm}")
    }
  }
  return(drivers)
}
