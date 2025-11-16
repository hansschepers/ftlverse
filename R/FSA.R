#' FSA
#' Forward sensitivity (derivative at each point on the trajectory)
#'
#' @export
FSA <- function(SIMS
                , dtime.s = seq(0, .6, .02)
                , yois = setdiff(names(SIMS$y_init00), c("x", "v"))
                , kpiOnly = FALSE){
  # SIMS <- runFunHES()
  dx <- sapply(dtime.s, \(tt) {
    cyclist05wrapper(SIMS = SIMS
                     , time = tt
                     , kpiOnly = kpiOnly)}
    , simplify = FALSE)
  ddx <- dx |> lapply(as.list) |> rbindlist(idcol = "time")
  ddx[, time := dtime.s]
  dim(ddx)
  ddx
  ddx
  nms <- names(ddx)
  nms
  yois2 <- setdiff(nms, yois)
  # yois <- nms
  {
    p0 <- pggs(SIMS$out
               , doMelt = TRUE
               , yois = yois
               , lwd = .1, lineAlpha = .4, lineColor = "red")
    p1 <- pggs(ddx
               , p = p0
               # , geom = "point"
               , doMelt = TRUE
               , yois = yois)
  }
  p1
  
}
