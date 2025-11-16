#' collectDifferences
#' @export
collectDifferences <- function(simList
                         , objectName = "drivers"
                         , focus = c("pb_driv", "pb_crop")
                         , yois = c(focus)
                         , doMelt = TRUE
                         , doCast = TRUE
                         , FUN = `-`
                         ){
  nms <- names(simList)
  if (is.null(nms)) nms <- paste0("sim", seq_along(simList))
  firstDT <- simList[[1]][[objectName]]
  factorsToKeep <- setdiff(aphFactors(firstDT), "processName")
  factorsToKeep <- c(factorsToKeep, aphTimes(firstDT))
  if (doMelt) firstDT <- aphMelt(firstDT)
  if ("processName" %in% names(firstDT)){
    if (!"all" %in% yois) firstDT <- firstDT[processName %in% yois]
  }
  if (doCast) firstDT <- hdcast(firstDT)
  aphKey(firstDT)
  # yois <- aphVariableLevels(firstDT)
  # firstDT[, c(yois)]
  # sapply(firstDT[, ..yois], class)
  # firstDT
  yois <- intersect(yois, aphVariableLevels(firstDT))
  diffList <- list()
  nn <- length(simList)
  ii <- 2
  for (ii in seq_along(simList)[-1]){
    log_trace("collectDifferences loop {ii}")
    nextDT <- simList[[ii]][[objectName]]
    yois <- intersect(yois, aphVariableLevels(nextDT))
    if (doMelt) nextDT <- aphMelt(nextDT)
    if ("processName" %in% names(nextDT)){
      if (!"all" %in% yois) nextDT <- nextDT[processName %in% yois]
    }
    if (doCast) nextDT <- hdcast(nextDT)
    aphKey(nextDT)
    differences <- mapply(FUN = FUN
                          , SIMPLIFY = FALSE
                          , nextDT[, ..yois]
                          , firstDT[, ..yois])
    setDT(differences)
    differences <- cbind(firstDT[, ..factorsToKeep], differences)
    # if ("Time" %in% names(firstDT)){
    #   differences$Time <- firstDT$Time
    # }
    diffList[[ nms[ii] ]] <- differences
  }
  return(diffList)
}
