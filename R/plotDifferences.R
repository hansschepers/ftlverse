#' plotDifferences
#' overlays pggs layered plots for a list of data
#' @export
plotDifferences <- function(diffList
                            , doMelt = TRUE
                            # , Time = seq(0, 350, 7)
                            , Time = diffList[[1]]$Time
                            , doScale = TRUE
                            , focus = c("pb_driv", "pb_crop")
                            , yois = aphKpis(focus)
                            , RS = NULL
                            , input = list()
                            , doplot = FALSE
                            # , extraFuns = c("hMAPE", "MAE", "RMSE", "hlength")
                            , ...){
  ii <- 1
  res <- list()
  p_diff <- ggplot2::ggplot()
  nn <- length(diffList)
  for (ii in seq_along(diffList)){
    log_trace("plotDifferences loop {ii}")
    tmp <- copy(diffList[[ii]])
    # if (length(Time)) {
    #   tmp$Time <- Time
    # }
    if (doMelt) tmp <- aphMelt(tmp)
    
    if (doScale) {
      if (!is.null(RS)){
        tmp <- tmp[processName %in% RS$processName]
      }
      tmp <- tmp[, value := scale01(value
                                    , yoi = processName
                                    , isDiff = TRUE
                                    , RS = RS)
                 , by = processName]
    }
    if ("processName" %in% names(tmp)){
      if (!"all" %in% yois) tmp <- tmp[processName %in% yois]
    }
    p_diff <- pggs(tmp
                   , p_diff = p_diff
                   , input = input
                   , doplot = FALSE
                   , ...)
    if (ii == nn){
      if (doplot) print(p_diff)
    }
  }
  return(p_diff)
}
