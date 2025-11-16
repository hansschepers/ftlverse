#' sweetSpot
#' @examples \dontrun{
#'   library(data.table)
#'   dtw <- data.table(
#'     dateTime = as.Date("2021-01-01") + lubridate::weeks(1:8)
#'   , plot_syn = "test"
#'   , cycle_syn = "testC"
#'   , plantLoad = c(10,100,150,30,90, 80, 70, 60)
#'   , stem.diam = c(7,9,11, 11, 9, 8, 7, 8))
#'   sweetSpot(dtw)
#' }
#' @export
sweetSpot <- function(dtwExtra){
  data.table::setDT(dtwExtra)
  dtwExtra <- copy(dtwExtra)
  dtSweetSpot <- data.frame(min = 0, max = 0)[0,]
  dtSweetSpot["plantLoad",] = c(110, 150)
  dtSweetSpot["stem.diam",] = c(8, 10.5)
  
  dtwExtra[, wk := lubridate::isoweek(dateTime)]
  dtwExtra[, plantLoad.wk := hmean(plantLoad), by = wk]
  dtwExtra[, stemDiam.wk := hmean(stem.diam), by = wk]
  
  dtwExtra[, label := ifelse(0 == wk%%5, as.character(wk, width = 2), "")]
  {
    dtwExtra[, stem.diam := aphApprox2(stem.diam), by = c(foi)]
    # dtwExtra[, stem.diam := findTails(stem.diam, which = "left", repl = "first"), by = c(foi)]
    dd <- aphSmooth(dtwExtra
                    , yois2smooth = c("plantLoad", "stem.diam")
                    , fois2smoothby = "plot_syn"
                    , reps = 3
                    , align = "left"
                    , padType = "tailValue"#c("mirror", "mean", "copy", "circular", "tailValue", "constant", "NA")[1]
                    # , na.rm = FALSE
    )
    # dtwExtra$plantLoad
    # dd$plantLoad
    # dtwExtra$stem.diam
    # dd$stem.diam
    
    input <-  list(xoi = "plantLoad", yoi = "stem.diam", geom = "pointline"
                   , xsc = c(0, 160), ysc = c(6, 12)
                   , xlab = "Generativity: plant load (fruits /m2)"
                   , ylab = "Strength: Stem Diameter (mm)"
                   , fsize = 16, legend = "right"
                   , chunkTitle = "KBB 2021 cycles: Plant Balance"
                   , doplot = FALSE)
    
    # the trajectory
    p1 <- pggs(dtwExtra, input = input, lwd = .1, psize = 2, pointAlpha = .3, lineAlpha = .3) 
    # the smoothed trajectory
    p1 <- pggs(dd, p = p1, input = input, lwd = 6, lineAlpha = .4, geom = "line") 
    # the 'orig' 'raw' mega week markers
    # p1 <- pggs(dtwExtra[0 == wk%%5], p = p1, input = input, geom = "point"
    #            , mega = TRUE, label = "label", psize = 6, pointAlpha = .2) 
    # the sweetRegion lines
    p1 <- pggs(dd[0 == wk%%5], input = input, geom = "point"
               , p = p1
               , hline = unlist(dtSweetSpot["stem.diam",])
               , vline = unlist(dtSweetSpot["plantLoad",])
               , ablinecolor = "orange", lwdFit = 1
               , expandX = c(0,0)
               , expandY = c(0,0)
               , mega = TRUE, label = "label", psize = 7, pointAlpha = 1) 
    # sweetRegion shaded rectangle
    p1 <- addRect(p1, xmin = dtSweetSpot["plantLoad","min"], xmax = dtSweetSpot["plantLoad","max"]
                  , ymin = dtSweetSpot["stem.diam","min"], ymax = dtSweetSpot["stem.diam","max"]
                  , fill="orange", alpha=0.15)
  }
  p1 +
    scale_x_continuous(limits =c(0,200), expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
    theme(axis.title.x = element_text(vjust = unit(-5, "char"))) +
    theme(axis.title.y = element_text(angle=90, vjust = 5)) +
    theme(plot.title=element_text(size=15, vjust=3)) +
    theme(plot.margin = unit(c(3, 4, 4, 5), "char"))
}
