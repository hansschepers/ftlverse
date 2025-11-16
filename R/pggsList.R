#' pggsList
#' 
#' @export
pggsList <- function(simList
                     , objectName = "cropLong"
                     , focus = "all"
                     , yois = aphKpis(focus)
                     , input1 = list(geom = "line"
                                     # , datebreaks = "quarters"
                                     , pointSize = 1
                                     , lwd = 1)
                     , inputLast = list(doplot = TRUE, chunkTitle = chunkTitle)
                     , chunkTitle = NULL
                     , doMelt = TRUE
                     , ...){
  if (is.data.frame(simList)){
    simList <- list(onlyRun = simList)
  }
  ii <- 1
  dd <- simList[[ii]][[objectName]]
  if (doMelt) dd <- aphMelt(dd)
  if ("processName" %in% names(dd)){
    if (!"all" %in% yois) dd <- dd[processName %in% yois]
  }
  
  dd
  p_2 <- pggs(dd, input = input1, ...)
  
  nn <- length(simList)
  for (ii in seq_along(simList)[-1]){
    message("sim: ", ii)
    dd2 <- simList[[ii]][[objectName]]
    if (doMelt) dd2 <- aphMelt(dd2)
    if ("processName" %in% names(dd2)){
      if (!"all" %in% yois) dd2 <- dd2[processName %in% yois]
    }
    if (ii == nn){
      input1 <- mergeParameters(input1, inputLast)
      # input1 <- mergeParameters(input1, list(chunkTitle = chunkTitle)
    }
    p_2 <- pggs(dd2
                , p = p_2
                , input = input1
                , lwd = 1, lineColor = "blue"#, lineAlpha = .1
                # , pointSize = 3, pointColor = "blue", pointAlpha = .1
                , ...)
    
  }
  p_2
}
# simList <- list(s1 = SIMS, s2 = SIMS2)
# pggsList(simList)
