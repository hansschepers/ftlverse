#' scale55
#' 
#' @export
scale55 <- function(x, newrange = c(-.5, .5), ...){
  scale01(x =x, newrange = newrange, ...)
}

#' scale01
#' 
#' modified default version of base
#' 
#' @param na.rm = TRUE 
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
scale01 <- function(x
                    , newrange = c(0, 1)
                    , oldrange = c(0, 0)
                    , RS = NULL
                    , time_resolution.oi = "hr"
                    , yoi = c("afw")[0]
                    , isDiff = FALSE
                    , na.rm = TRUE
                    , clip = FALSE) {
  if (length(yoi)){
    oldrange <- getRS(RS
                      , time_resolution.oi = time_resolution.oi
                      , yois = yoi
                      , asRange = TRUE)
  }
  if (any(is.na(oldrange))) return(x)
  if(okData(x) < 1e-9){
    return(rep(0, length(x)))
  }
  if(hdiffrange(x) < 1e-9){
    return(rep(1, length(x)))
  }
  if(diff(oldrange) < 1e-9){
    mi <- min(x, na.rm = na.rm)
    ma <- max(x, na.rm = na.rm)
  } else {
    mi = oldrange[1]
    ma = oldrange[2]
  }
  if (isDiff){
    y <- x / diff(oldrange)
    return(y)
  }
  y <- (x - mi) / (ma - mi)
  y <- y * diff(newrange) + newrange[1]
  if (clip) y = pmax(y, newrange[1])
  if (clip) y = pmin(y, newrange[2])
  y
}
