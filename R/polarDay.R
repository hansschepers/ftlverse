#' polarDay
#' 'cause and effect' analysis
#' if X precedes / leads Y, the 'movement in scatter is counter-clockwise,
#' so with negative d_angle
#' @examples \dontrun{
#'   dt <- data.table(hr = seq(100))
#'   dt[, RAD := 200 + 180*sin(3 *2*pi*hr/100)]
#'   dt[, GHTEMP := 20 + 5 * sin(3 *2*pi*hr/100 - .3)]
#'   ppggs(dt, xoi = "RAD", yoi = "GHTEMP")
#'   dtp <- polarDay(dt, thr = 1
#'                     , center = c(200, 20)
#'                      )
#'   hprettyNum(dtp)
#'   ppggs(melt(dtp, id.vars = "hr"))
#' }
#' 
#' @export
polarDay <- function(dt
                     , xoi = "RAD"
                     , yoi = "GHTEMP"
                     # , center = c(0, 0)
                     , center = "scale"
                     , bycols = character(0)
                     , ...
){
  dtp <- copy(dt)
  # scale ----
  if (center[1] == "scale"){
    dtp[, (c("xc", "yc")) := list(x = as.numeric(hmean(get(xoi)))
                                 , y = as.numeric(hmean(get(yoi))))
       , by = bycols]
  } else {
    dtp[, (c("xc", "yc")) := as.list(center)]
  }
  dtp[, x := get(xoi) - xc]
  dtp[, y := get(yoi) - yc]
  
  dtp[, radius := sqrt(x*x + y*y)]
  dtp[, angle  := atan2(x, y)]
  dtp[, d_angle := angleDiff(angle), by = c(bycols)]
  dtp[, d_angle2 := hclamper2(diff0(angle), ...), by = c(bycols)]
  dtp[]
}


if(F){
  # p_lines <- pggs(dd, xoi = xoi, yoi = yoi, foi = "doy", facet_g = "ss ~ plot_syn"
  #                 , legend = "none", geom = "pointline"
  #                 , mega = TRUE, label = "hr", doplot = TRUE, chunkTitle = "trajectory")
  rm(ddd)
  ddd <- polarDay(dtw  # [doy %in% c(75, 130, 180)]
                  , xoi = "VADEFE"
                  , yoi = "GHTEMP"
                  # , center = "scale"
                  , center = c(-1, 5)
  )
  # xoi <- "radius"
  # yoi <- "angle"
  # yoi <- "d_angle"
  pggs(ddd[wk %in% seq(4, 34, 2)]
       , xoi = "radius"
       # , xoi = "hr"
       # , yoi = "d_angle"
       , yoi = "angle"
       , group = c(foip, "doy")
       , facet_w = "wk", free_y = FALSE
       # , xtics = 6
  )
}

if(F){
  
  # dd[, tod := timeOfDay(dateTime)]
  keep <- c(foip, doi, "doy", xoi, yoi, "hr", "ss")
  # "growthRate"
  range(dd$hr)
  dd <- copy(ddd)
  dd2 <- dd[, ..keep]
  dd2[, doy := as.factor(doy)]
  range(dd2$hr)
  dd2 <- aphMelt(dd2)
  dd2s <- aphSmooth(hdcast(dd2)
                    , yois2smooth = c("d_angle", "radius")
                    , fois2smoothby = c("plot_syn", "ss", "doy")
                    , n = 3
                    , reps = 3
                    , padType = "circular")
  dd2s <- aphMelt(dd2s)
  aphKey(dd2s)
  dd2s
  range(dd2s$hr)
  # setkeyv(dd2, c(foip, voi, "ss", "doy", "dDate", "tod"))
  p_lines <- pggs(dd2s, xoi = "hr", foi = foip, group = c(foip, "ss")
                  , xtics = 6, hline = 0
                  , geom = "pointline"
                  , doplot = TRUE, chunkTitle = "Angular Speed and scaled Radius of dayProfile GHTEMP(RAD)")
  # vrmd("docuFun")
  # vrmd("renderview")
}
# p_lines <- pggs(dd, xoi = xoi, yoi = yoi, foi = "doy", facet_g = "ss ~ plot_syn"
#                 , legend = "none", geom = "pointline", free_y = FALSE
#                 , mega = TRUE, label = "hr", doplot = TRUE, chunkTitle = "trajectory")
# 
