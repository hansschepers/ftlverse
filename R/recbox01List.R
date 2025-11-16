#'  recbox01List
#'  @examples \dontrun{
#'    pp <- recbox01Parms()
#'    hprettyNum(pp)
#'    pp2 <- recbox01List(pp)
#'    hprettyNum(pp2)
#'    compareNames(pp, pp2)
#'    
#'    pp1 <- mergeParameters(pp, list(IR = 100))
#'    hprettyNum(pp1)
#'    compareNames(pp, pp1)
#'    
#'    pp3 <- recbox01List(pp1)
#'    hprettyNum(pp3)
#'    compareNames(pp1, pp3)
#'    compareLists(pp2, pp3)
#'    
#'    # in one go
#'    pp3a <- recbox01List(editList = list(IR = 100))
#'    compareNames(pp3, pp3a)
#'    compareLists(pp3, pp3a)
#'    cbind(pp3, pp3a)
#'    
#'    
#'    pp4 <- recbox01List()
#'    dd <- data.table(gc = 10^(seq(-2, 2, length.out = 51)))
#'    dd[, (names(pp4)) := recbox01List(editList = list(gc = gc, TT = 3))]
#'    dd
#'    pggs(dd, xoi = "gc", yoi = "M_c", logx = TRUE)
#'    pggs(aphMelt(dd), xoi = "gc", logx = TRUE, fsize = 8)
#'    
#'    pp5 <- recbox01List()
#'    dd <- data.table(gt = 10^(seq(-2, 2, length.out = 51)))
#'    dd[, (names(pp3a)) := recbox01List(editList = list(gc = gc, TT = 3))]
#'    dd
#'    pggs(dd, xoi = "gc", yoi = "M_c", logx = TRUE)
#'    pggs(aphMelt(dd), xoi = "gc", logx = TRUE, fsize = 8)
#'  }
#'  
#'  @export
recbox01List <- function(parmsUsed = recbox01Parms()
                         , editList = list()
                         , time = NA
                         , y = NA
                         , conserve = TRUE){
  
  parmsUsed <- mergeParameters(parmsUsed, editList)
  list2env(parmsUsed, envir = environment())
  
  # kinetics
  k1 <- k_1/ (L1*IR)
  k2 <- FF * k_1
  k_2 <- k2 * L2/IR
  
  u <- (k1  + k2  * gc)      / (1 + gc)
  v <- (k_1 + k_2 * gc * cc) / (1 + gc * cc)
  rss <- v / (u + v)
  # 
  a <- (a1 + a2 * gc)      / (1 + gc)
  b <- (a4 + a3 * gc * cc) / (1 + gc * cc)
  M_c <- a * rss + b * (1 - rss)
  
  if (conserve){
    g1 <- gc + (gc - g0) / beta
  } else {
    g1 <- gc
  }
  tau0 <- TT / (1 + beta)
  tau1 <- tau0 * beta
  
  # stimulus
  if (all(!is.na(time))){
    gt <- blockPulse(time = time
                     , pp = list(TT = TT, beta = beta, gc = gc, g0 = g0)
                     , conserve = conserve)
  }
  
  g1 <- gc + (gc - g0) / beta
  tau0 <- TT / (1 + beta)
  tau1 <- beta * tau0
  
  u0 <- (k1  + k2  * g0)      / (1 + g0)
  v0 <- (k_1 + k_2 * g0 * cc) / (1 + g0 * cc)
  u1 <- (k1  + k2  * g1)      / (1 + g1)
  v1 <- (k_1 + k_2 * g1 * cc) / (1 + g1 * cc)
  taua0 <- u0 + v0
  taua1 <- u1 + v1
  omega0 <- exp(-tau0 * taua0)
  omega1 <- exp(-tau1 * taua1)
  
  # average receptor state
  rss0 <- v0 / taua0
  rss1 <- v1 / taua1
  Rp0 <- (omega0*(rss1 * (1 - omega1) - rss0) + rss0) / (1 - omega1 * omega0)
  Rp1 <- (omega1*(rss0 * (1 - omega0) - rss1) + rss1) / (1 - omega1 * omega0)
  
  OO1 <- tau1 * rss1
  OO2 <- (Rp1 - rss1) * (1 - omega1) / taua1
  OO3 <- tau0 * rss0
  OO4 <- (Rp0 - rss0) * (1 - omega0) / taua0
  R_p <- (OO1 + OO2 + OO3 + OO4) / TT
  
  # average Activity
  aa0 <- (a1 + a2 * g0)      / (1 + g0)
  bb0 <- (a4 + a3 * g0 * cc) / (1 + g0 * cc)
  aa1 <- (a1 + a2 * g1)      / (1 + g1)
  bb1 <- (a4 + a3 * g1 * cc) / (1 + g1 * cc)
  Ass0 <- aa0 * rss0 + bb0 * (1 - rss0)
  Ass1 <- aa1 * rss1 + bb1 * (1 - rss1)
  
  Ap0 <- aa0 * Rp1 + bb0 * (1 - Rp1)
  Ap1 <- aa1 * Rp0 + bb1 * (1 - Rp0)
  
  OO1a <- tau1 * Ass1
  OO2a <- (Ap1 - Ass1) * (1 - omega1) / taua1
  OO3a <- tau0 * Ass0
  OO4a <- (Ap0 - Ass0) * (1 - omega0) / taua0
  M_p <- (OO1a + OO2a + OO3a + OO4a) / TT
  # gain
  G_p <- M_p / M_c
  # Li & Goldbeter
  aMstep <- (rss0 - rss1) * (aa1 - bb1)
  zeta <- (1 - omega1) * (1 - omega0) / (1 - omega1 * omega0)
  
  toReturn <- c("M_p", "R_p", "Rp0", "Rp1", "rss", "rss0", "rss1", "gc", "M_c")
  
  if (all(!is.na(y))){
    r <- y#[1]
    ar <- (a1 + a2 * gt)      / (1 + gt)
    br <- (a4 + a3 * gt * cc) / (1 + gt * cc)
    A_r <- ar * r + br * (1 - r)
    # ravg <- hmean(r)   # same as R_p
    # Aavg <- hmean(A_r) # same as M_p
    M_p2 <- hmean(A_r*A_r)
    toReturn <- c(toReturn, c("M_p2"))    
  } else {
    
    r <- Rp0
    
  }
  dr <- v - (u + v) * r
  
  notToReturn <- c("parmsUsed", "conserve", "editList", "y", "notToReturn")
  if (all(is.na(time))) notToReturn <- c(notToReturn, "time")
  mget(setdiff(toReturn, notToReturn ))
}