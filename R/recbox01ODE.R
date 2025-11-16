#' recbox01ODE
#' 
#' @export
recbox01ODE <- function(time = 0
                        , y = NA
                        , pa = recbox01Parms()
                        , driv_funs = list(time = list())
                        , aux = FALSE
) {
  # drivers ************************************************ ##################
  # .driv_funs
  for (poi in names(driv_funs$time)){
    pa[poi] <- driv_funs$time[[poi]](v = time)
  }
  
  parmsUsed <- recbox01par2par(pa, time = time)
  list2env(parmsUsed, envir = environment())
  u <- (k1  + k2  * gt)      / (1 + gt)
  v <- (k_1 + k_2 * gt * cc) / (1 + gt * cc)
  
  # normally set by wrapper runFunRB...  
  if (!is.na(y)){
    r <- y[1]
  } else {
    kpis <- recbox01List(parmsUsed = parmsUsed)
    r <- kpis$Rp1
  }
  
  dr <- v - (u + v) * r
  
  if (!aux) return(list(dr))
  
  a <- (a1 + a2 * gt)      / (1 + gt)
  b <- (a4 + a3 * gt * cc) / (1 + gt * cc)
  Act <- a * r + b * (1 - r)
  rss <- v / (u + v)
  
  list(dr, c(gt = gt
             , u = u
             , v = v
             , a = a
             , b = b
             , Act = Act
             , rss = rss)
  )
}
