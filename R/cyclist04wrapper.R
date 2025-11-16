#' cyclist04wrapper
#' @export
cyclist04wrapper <- function(x = SIMS$parmsUsed
                             , SIMS
                             , time = 0
                             , y = SIMS$y_init00
                             , driv_funs = SIMS$driv_funs
                             , CONSTANTS = SIMS$CONSTANTS){
  
  if (time > 0){
    ind <- findInterval(time, unlist(SIMS$out[, time]))
    nms <- names(y)
    y = unlist(as.data.table(SIMS$out)[ind, ..nms])
  }
  # derivs <- cyclist05(time = time
  #                     , y = y
  #                     , pa = x
  #                     , driv_funs = driv_funs
  #                     , CONSTANTS = CONSTANTS
  # )
  derivs <- cyclist04(time = time
                      , y = y
                      , pa = x
                      # , driv_funs = driv_funs
                      # , CONSTANTS = CONSTANTS
  )
  derivs0 <- setNames(as.numeric(derivs[[1]]), names(SIMS$y_init00))
  c(derivs0, derivs[[2]])
}

