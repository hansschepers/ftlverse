#'  recbox01Alg
#'  @examples \dontrun{
#'    pggs(aphMelt(recbox01Alg(), fois = "gc"), logx = TRUE)
#'  }
#'  
#'  @export
recbox01Alg <- function(out = data.frame()
                        , times = seq(0, 5*parms$TT, length.out = 1001)
                        , parms = recbox01Parms()
                        , timeSeries = FALSE){
  # parmsUsed <- .parmsUsed
  # parmsUsed <- recbox01par2par(parms)
  list2env(parms, envir = environment())
  
  if (!length(out)){
    if (timeSeries){
      ################################################################## continuousout <- blockPulse(time = seq(0, 1, length.out = 1001)
      out <- data.table(time = times)
    } else {
      ################################################################## continuous
      out <- data.table(igc = seq(-2, 2, length.out = 51))
    }
  }
  if (timeSeries & !"gc" %in% names(out)){
    out[, gc := blockPulse(time
                           , pp = list(TT = TT, beta = beta
                                       , gc = gc, g0 = g0))]
  }
  if ((!timeSeries) & !"gc" %in% names(out)){
    out[, gc := 10^igc]
  }
  .out0 <<- copy(out)
  
  out[, k1 := k_1 / (L1 * IR)]
  out[, k2 := FF * k_1]
  out[, k_2 := k2 * (L2 / IR)]  ######### /FF ?????????????
  
  out[, u := (k1  + k2  * gc)      / (1 + gc)]
  out[, v := (k_1 + k_2 * gc * cc) / (1 + gc * cc)]
  out[, rss := v / (u + v)]
  
  out[, a := (a1 + a2 * gc)      / (1 + gc)]
  out[, b := (a4 + a3 * gc * cc) / (1 + gc * cc)]
  out[, M_c := a * rss + b * (1 - rss)]
  # irreversible limit
  out[, A_IR := gc / (1 + gc)^2]
  
  ################################################################## pulsatile
  out[, g1 := gc + (gc - g0) / beta]
  out[, tau0 := TT / (1 + beta)]
  out[, tau1 := beta * tau0]
  
  out[, u0 := (k1  + k2  * g0)      / (1 + g0)]
  out[, v0 := (k_1 + k_2 * g0 * cc) / (1 + g0 * cc)]
  out[, u1 := (k1  + k2  * g1)      / (1 + g1)]
  out[, v1 := (k_1 + k_2 * g1 * cc) / (1 + g1 * cc)]
  out[, taua0 := u0 + v0]
  out[, taua1 := u1 + v1]
  out[, omega0 := exp(-tau0 * taua0)]
  out[, omega1 := exp(-tau1 * taua1)]
  
  # average receptor state
  out[, rss0 := v0 / taua0]
  out[, rss1 := v1 / taua1]
  out[, Rp0 := (omega0*(rss1 * (1 - omega1) - rss0) + rss0) / (1 - omega1 * omega0)]
  out[, Rp1 := (omega1*(rss0 * (1 - omega0) - rss1) + rss1) / (1 - omega1 * omega0)]
  
  out[, OO1 := tau1 * rss1]
  out[, OO2 := (Rp1 - rss1) * (1 - omega1) / taua1]
  out[, OO3 := tau0 * rss0]
  out[, OO4 := (Rp0 - rss0) * (1 - omega0) / taua0]
  out[, R_p := (OO1 + OO2 + OO3 + OO4) / TT]
  
  # average Activity
  out[, aa0 := (a1 + a2 * g0)      / (1 + g0)]
  out[, bb0 := (a4 + a3 * g0 * cc) / (1 + g0 * cc)]
  out[, aa1 := (a1 + a2 * g1)      / (1 + g1)]
  out[, bb1 := (a4 + a3 * g1 * cc) / (1 + g1 * cc)]
  out[, Ass0 := aa0 * rss0 + bb0 * (1 - rss0)]
  out[, Ass1 := aa1 * rss1 + bb1 * (1 - rss1)]
  
  out[, Ap0 := aa0 * Rp1 + bb0 * (1 - Rp1)]
  out[, Ap1 := aa1 * Rp0 + bb1 * (1 - Rp0)]
  
  out[, OO1a := tau1 * Ass1]
  out[, OO2a := (Ap1 - Ass1) * (1 - omega1) / taua1]
  out[, OO3a := tau0 * Ass0]
  out[, OO4a := (Ap0 - Ass0) * (1 - omega0) / taua0]
  out[, M_p := (OO1a + OO2a + OO3a + OO4a) / TT]
  # gain
  out[, G_p := M_p / M_c]
  # Li & Goldbeter
  out[, aMstep := (rss0 - rss1) * (aa1 - bb1)]
  out[, zeta := (1 - omega1) * (1 - omega0) / (1 - omega1 * omega0)]
  
  .out <<- copy(out)
  out[]
}
# pggs(out, doMelt = T)
