#' plotPrediction
#' 
#' @export
plotPrediction <- function(
  dtlong
  , doplotShow = TRUE
  , panels.oi = c("afw", "yield", "hmfromtemp", "tempsum"
                  , "trussspeed", "temp", "pruning"
                  , "plantload", "strength", "surplus"
                  , "fruitsetharvest"
                  , "harvest", "setting"
                  , "asted"
                  , "maturity", "stem.diam")
  , pattern = c("nyield"
                , "lz", "wkavg"
                , "asted"
                , "cum", "density", "diam", "stem", "setting", "shifted", "withtails", "CHECK"
                , "strength"
                , "hmfromTemp"
                , "maturity"
                , "yield"
                , "plantload"
                , "trussspeed"
                , "OUTEMP", "avtemp3", "avtemp9"
                , "tempSum"
                , "temp"
                , "afw"
                , "harvest")
  , yGroups = list(pruning = c("pruning", "stem.density.setting")
                   , surplus = c("sink", "source")
                   , fruitsetharvest = c("setting", "harvest")
  )
  , status.oi = "all"
  , panels.out = c("check", "GHCO2C", "ECIRRI", "RAD")
  , foic = "cycle_syn"
  , input = list(fsize = 12
                 # , facet_w = "panel"
                 , geom = "pointline"
                 , pointAlpha = .6
                 , xlab = NULL
                 , xsize = 5
                 , margin = c(0,0,0,0)
                 , lwd = 2
                 )
  , megaValue = FALSE
  , chunkTitle = ""
  , voi = "processName"
  , foi = c(voi, "status")[2]
  , facet_w = "panel"
  , minDate = as.Date("2000-03-01")
  , meltFirst = !"processName" %in% names(dtlong)
  , ...
){
  if (meltFirst){
    dtmp0 <- aphMelt(dtlong)
  } else {
    dtmp0 <- copy(dtlong)
  }
  # dtmp0[, .N, by = processName]
  # dtmp0[grepl("setting", processName), .N, by = c(voi)]
  # dtmp0[grepl("yield", processName), .N, by = c(voi)]
  
  # hdcast(dtmp0[grepl("(fruit)|(afw)|(yield)", processName) & cycle_syn == "cycle_21"])
  # hdcast(dtmp0[grepl("(setting)|(trussSpeed)|(pruning)", processName) & cycle_syn == "cycle_21"])
  # table(dtmp0$processName)
  # patterns in UPPERCASE are matched as lowercase!
  if (megaValue) {
    input <- mergeParameters(input
                             , list(mega = TRUE, label = "value"
                                    , labelDigits = 0, psize = 8, labelSize = 3)
    )
  }
  .dtmp0 <<- dtmp0
  dtmp <- addYgroups(dtmp0
                     , yGroups = yGroups
                     , pattern = pattern)
  keys <- aphKey(dtmp)
  .dtmp <<- dtmp
  # dtmp[grepl("diam", processName, ignore.case = TRUE)]
  # dtmp[grepl("hmFr", processName, ignore.case = TRUE)]
  patternMatches <- attr(dtmp, "patternMatches")
  patternMatches
  # attr(dtmp, "patternMatches") <- NULL
  
  # dtmp[, color := as.character(as.integer(as.factor(as.character(processName))))
  # , by = c("panel", foic)]
  (keys2 <- aphKey(dtmp))
  dtmp <- dtmp[!panel %in% panels.out]
  # p <- pggs(dtmp, foi = "color", group = foic
  #           , geom = "pointline", pointAlpha = .3, lwd = .3)
  # htable(dtmp, lhs = voi, rhs = "panel", long = FALSE)
  # setkeyv(dtlong, c(foic, "dateTime"))
  aphKey(dtmp)
  # dtmp[, .N, by = c("panel")]
  
  dtmp <- addProcessNameStatus(dtmp)
  if (panels.oi[1] != "all") {
    dtmp <- dtmp[panel %in% panels.oi]
  }
  if (status.oi[1] != "all") {
    dtmp <- dtmp[status %in% status.oi]
  }
  .dtmpPred <<- copy(dtmp)
  p2 <- panelPlot(dtmp[dateTime > minDate]
             , foi = foi
             , facet_w = facet_w
             , group = c(foic, voi, "phase")
             , doplot = doplotShow
             , chunkTitle = paste(htimestamp(), chunkTitle)
             , input = input
             , ...
  )
  return(p2)
}
