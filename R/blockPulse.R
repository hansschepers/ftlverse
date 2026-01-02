#' blockPulse
#' @examples \dontrun{
#'    dd <- blockPulse(time = seq(0, 100, length.out = 1001)
#'       , pp = list(TT = 10, beta = 0.25, gc = 1, g0 = 0)  , doplot = T)
#'    dd <- blockPulse(time = seq(0, 100, length.out = 1001)
#'       , pp = list(TT = 10, beta = 1, gc = 1, g0 = 0.8), doplot = T)
#'    dd <- blockPulse(time = seq(0, 1, length.out = 1001)
#'       , pp = list(TT = .1, beta = 1, gc = 1, g0 = 0.8), doplot = T)
#' }
#' @export
blockPulse <- function(time = seq(0, 100, 1)
                       , pp = list(TT = 10, beta = 0.25
                                     , gc = 1, g0 = 0)
                       , conserve = TRUE
                       , doplot = F
                       ){
  tau0 <- pp$TT / (1 + pp$beta)
  tau1 <- pp$beta * tau0
  if (conserve){
    g1 <- pp$gc + (pp$gc - pp$g0) / pp$beta
  } else {
    g1 <- pp$gc
  }
  
  gt <- pp$g0 + (g1 - pp$g0) * (time %% pp$TT < tau1)
  if (doplot){
    print(ppggs(data.frame(x = time, y = gt), title = list2title(pp)))
  }
  gt
}
