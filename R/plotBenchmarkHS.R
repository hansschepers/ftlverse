#' plotBenchmark
#' 
#' @export
plotBenchmark <- function(dt.front
                          , dt.frontf
                          , xoi = "hod", voi = "processName"
                          , foi = "plot_syn"
                          , doplot = FALSE
                          , pggsInput = list(lineAlpha = 0.5
                                             # , xtics = 12, xsc = c(0,24), xlab = "hour of the Day"
                                             # , fsize = 16
                          )
                          , title = "Strabena: Focus plot compared to Day-Highest and Day-Lowest"
                          , ...){
  
  aphKey(dt.front)
  aphKey(dt.frontf)
  
  dt.front[, plot_synCoded := paste(foi
                                    , as.numeric(as.factor(get(foi))))]
  foi <- union(foi, "plot_synCoded")
  
  bycolsTmp <- c(union(foi, setdiff(aphFactors(dt.front), "mon")))
  
  dt.frontMean <- dt.front[, .(value = hmean(value)), by = bycolsTmp]
  print(dt.frontMean)
  log_info("plotting")
  
  lwd = 2
  
  p0 <- pggs(dt.front
             , xoi = xoi
             , foi = foi
             , input = pggsInput
             , doplot = FALSE
             , lwd = 0.5
  )
  # ribbon ----
  {
    lbub <- dt.front[, .(ymax = hmax(value)
                         , ymin = hmin(value))
                     , by = c(xoi, voi)]
    dt.front <- lbub[dt.front, on = c(voi, xoi)]
    p1 <- pggs(lbub, xoi = xoi, yoi = "ymin", foi = voi
               , geom = "ribbon", ribbonColor = "grey"
               , p = p0
               , input = pggsInput
               , doplot = FALSE
    )
  }
  # min max for day average ----
  {
    # fois <- aphFactors(dt.front)
    # sapply(dt.front[, ..fois], unique)
    dayAvg <- dt.front[, .(ymean = hmean(value))
                       , by = c(foi, voi)]
    dayAvgLBUB <- dayAvg[, .(ymax = hmax(ymean), ymin = hmin(ymean)), by = c(voi)]
    dd <- dayAvgLBUB[dayAvg, on = voi]
    dd[, highest := ymax == ymean]
    dd[, lowest := ymin == ymean]
    # options(digits = 2)
    # hdcast(dd, lhs = "processName", rhs = "plot_synCoded", value.var = "ymean")
    # hdcast(dd, lhs = "processName", rhs = "plot_synCoded", value.var = "highest")
    # hdcast(dd, lhs = "processName", rhs = "plot_synCoded", value.var = "lowest")
    
    hh <- dd[highest == TRUE, .(maxPlot = first(plot_synCoded)), by = c(voi)]
    dt.h <- hh[dt.front, on = c(voi)]
    dt.h <- dt.h[maxPlot == plot_synCoded]
    
    p1h <- pggs(dt.h, xoi = xoi, lineColor = "green", lwd = lwd
                , p = p1
                , input = pggsInput
                , doplot = FALSE    )
    
    hh <- dd[lowest == TRUE, .(minPlot = first(plot_synCoded)), by = c(voi)]
    dt.lw <- hh[dt.front, on = c(voi)]
    dt.lw <- dt.lw[minPlot == plot_synCoded]
    p1lwh <- pggs(dt.lw
                  , xoi = xoi
                  , lineColor = "red", lwd = lwd
                  , input = pggsInput
                  , p = p1h
                  , doplot = FALSE)
  }
  p1lwh
  lwd = lwd - 1
  p0f <- pggs(dt.frontf
              , p = p1lwh
              , xoi = xoi, foi=foi
              , input = pggsInput
              # , lwd = lwd + 1
              , doplot = doplot
              , title = title
  )
  p0f
}