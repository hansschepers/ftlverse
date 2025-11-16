#' apggs
#' 
#' @export
apggs <- function(
  dfg
  , statusCol = "status"
  , keep = c("plot_syn", "status", statusCol, "modelId", "plot_syn", "cycle_syn")
  #
  , yGroups = list()
  , pattern = character(0)
  , panels.oi = "all"
  , panel.out = "rest" # character(0)
  , removeZeroRange = TRUE
  , restInRest = TRUE
  #
  , status.oi = "all"
  , doplot = TRUE
  , bycols = intersect(c("plot_syn", "cycle_syn"), names(dfg))
  , foi = c(variable.name, "status")[1]
  , facet_w = "panel"
  , input = list(fsize = 10
                 , geom = "pointline"
                 , pointAlpha = .4
                 , lwd = 1
                 , xsize = 7
                 , xlab = NULL)
  , margin = c(1,0,1,2)
  , megaValue = FALSE
  , variable.name = "processName"
  , group = "status"
  , minDate = -Inf # as.Date("2000-03-01")
  , meltFirst = !"processName" %in% names(dfg)
  , more = "none"
  , ...
){
  group = union(group, c(bycols, variable.name))
  # , yois = aphVariableLevels(dfg)
  # , apattern = coreVars(yois, sep = c("[_\\.~]", "[042_\\.~]")[2])
  # yoisStems <- unique(sapply(strsplit(yois, "[042_\\.]"), `[[`, 1))
  # dtmf[grepl("AFW_slow", processName)]
  # dtmf <- addYgroups(dtmf, panel.out = "d") # , pattern = setdiff(c(yoisStems), "d")
  # dtmf[grepl("AFW", processName)]
  
  if (megaValue) {
    input <- mergeParameters(input
                             , list(mega = TRUE
                                    , label = "value"
                                    , labelDigits = 0
                                    , psize = 8
                                    , labelSize = 3)
    )
  }
  
  if (meltFirst){
    log_trace("melting... (in apggs())")
    dtmp <- aphMelt(dfg)
  } else {
    dtmp <- copy(dfg)
  }
  .dtmpLong <<- copy(dtmp)
  # dtmp <- copy(.dtmpLong)
  if (!statusCol %in% names(dtmp)){
  # add status column ----
    dtmp <- addProcessNameStatus(dtmp)
  }
  
  
  if (status.oi[1] != "all") {
    dtmp <- dtmp[status %in% status.oi]
  }
  
  dtmp <- dtmp[dateTime > minDate]
  # dtmp <- addYgroups(dtmp, pattern = "densi")
  # add panel column ----
  dtmp <- addYgroups(dtmp
                     , yGroups = yGroups
                     , pattern = pattern
                     , removeZeroRange = removeZeroRange
                     , panel.out = panel.out
                     , restInRest = restInRest
                     , variable.name = variable.name
                     )
  if (!"all" %in% panels.oi) {
    dtmp <- dtmp[panel %in% panels.oi]
  }
  # add 'color' (1,2,3) within panels
  dtmp[, color := as.character(as.integer(as.factor(as.character(processName))))
       , by = c("panel", bycols)]
  
  keys <- aphKey(dtmp)
  .dtmp <<- dtmp
  patternMatches <- attr(dtmp, "patternMatches")
  .patternMatches <<- patternMatches
  
  (keys2 <- aphKey(dtmp))
  if (F){
    ddtab <- htable(dtmp
                    , lhs = c("status", variable.name, "color")
                    , rhs = "panel", long = FALSE)
    print(ddtab)
  }
  .dtmpPred <<- copy(dtmp)
  
  p2 <- panelPlot(dtmp
             , foi = foi
             , facet_w = facet_w
             , group = group
             , keep = keep
             , doplot = doplot
             , margin = margin
             , input = input
             , more = more
             , ...
  )
  attr(p2, "patternMatches") <- patternMatches
  return(p2)
}



#' mpgg
#' 
#' @export
mpggs <- function(dfg
                  , mfois
                  , hsep = "_"
                  , preTitle = c("none", "fois", "counter")[1]
                  , drop = T
                  , title = "ignored"
                  , foisFixed = NULL
                  , ...){
  if (preTitle == "none") prefixTitle = NULL
  mfois.title = paste(mfois, collapse=", ")
  if (preTitle == "fois") prefixTitle = paste0(mfois.title,": ")
  if (length(mfois) > 1) {
    # MonVeg1::
    dfg = haddKey(dfg, mfois, sep = hsep, keyID="mfois")
    mfois = "mfois"
  }
  pp = list()
  ii = 1
  (level.s = sort(unique(as.data.frame(dfg)[, mfois]) ) )
  print(level.s)
  sel.oi = level.s[1]
  for (sel.oi in level.s){
    dfgs = dfg[ dfg[,mfois]==sel.oi, ]
    if (drop) dfgs = droplevels(dfgs)
    dfgs <- fixFactorDF(dfgs, foisFixed) 
    plotcounter = paste0("Plot#",ii,", ")
    if (preTitle == "counter") prefixTitle = paste0(plotcounter,": ")
    p = pggs(dfgs, title=paste0(prefixTitle,sel.oi), ... )
    # p = pggs(dfgs, title=paste0(prefixTitle,sel.oi), ... )
    pp[[ii]] = p
    ii = ii + 1
  }
  ii = ii - 1
  print(paste0("number of plots:",ii))
  # hsarrpp(pp)
  return(pp)
}
