#'  recbox01Parms
#' @export
recbox01Parms <- function(){
  list(gc = 1, g0 = 0, TT = 0.1, beta = 1   # stimulus
       , k_1 = 1, L1 = 10, L2 = 0.1, IR = 1, FF = 1, cc = 1  # kinetics
       , a1 = 0, a2 = 1, a3 = 0, a4 = 0)   # activities
}



#' recbox01par2par
#' @examples \dontrun{
#'    pp <- recbox01Parms()
#'    hprettyNum(pp)
#'    pp2 <- recbox01par2par(pp)
#'    hprettyNum(pp2)
#'    compareNames(pp, pp2)
#'    
#'    pp1 <- mergeParameters(pp, list(IR = 100))
#'    hprettyNum(pp1)
#'    compareNames(pp, pp1)
#'    
#'    pp3 <- recbox01par2par(pp1)
#'    hprettyNum(pp3)
#'    compareNames(pp1, pp3)
#'    
#'    compareLists(pp2, pp3)
#' }
#' @export
recbox01par2par <- function(parmsUsed
                            , time = NA
                            , conserve = TRUE
                            , enrichParms = FALSE){
  
  list2env(parmsUsed, envir = environment())
  # kinetics
  k1 <- k_1/ (L1*IR)
  k2 <- FF * k_1
  k_2 <- k2 * L2/IR
  
  if (!is.na(time)) enrichParms <- TRUE
  if(enrichParms){
    
    if (conserve){
      g1 <- gc + (gc - g0) / beta
    } else {
      g1 <- gc
    }
    tau0 <- TT / (1 + beta)
    tau1 <- tau0 * beta
    
    
    u <- (k1  + k2  * gc)      / (1 + gc)
    v <- (k_1 + k_2 * gc * cc) / (1 + gc * cc)
    rss <- v / (u + v)
    # 
    a <- (a1 + a2 * gc)      / (1 + gc)
    b <- (a4 + a3 * gc * cc) / (1 + gc * cc)
    M_c <- a * rss + b * (1 - rss)
    
    # stimulus
    if (!is.na(time)){
      gt <- blockPulse(time = time
                       , pp = list(TT = TT, beta = beta, gc = gc, g0 = g0)
                       , conserve = conserve)
    }
    
  }
  
  mget(setdiff(ls(), c("parmsUsed", "conserve", "enrichParms", "time") ))
}
