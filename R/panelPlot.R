#' panelPlot
#' @export
panelPlot <- function(
    dt
    , processGroups = list(Fruits = c("harvested.fruits", "setting.pred")
                           , Production = c("yield.cu", "trussSpeed.cu"))[1:2]
    , yois = "all"
    , sim2display = NULL
    , doOrdered = TRUE
    , input = list()
    , inputBase = list(xlab = NULL
                       , xsize = 9
                       , margin = c(1,0,1,1)
                       , legendSize = 8
                       , foi = "processName"    )
    , hash = "none"
    , doplot = FALSE
    , facet_w = "none"
    , panelCols = NULL, panelRows = NULL
    , allowppt = TRUE
    , chunkTitle = NULL
    , legend = "auto"
    , more = "none"
    , legendRows = 4
    , patchworkOperator = "+"
    , ...){
  
  input <- mergeParameters(inputBase, input)
  
  dt <- copy(as.data.table(dt))
  dt[, processName := as.character(processName)]
  if (!"all" %in% yois){
    dt <- dt[processName %in% yois]
  }
  
  if (!is.null(sim2display)){
    log_debug("using sim2display for data...")
    # dt[processName %in% names(sim2display)
    #     , processName := sim2display[as.character(processName)]]
    # dt[, processName := fixFactor(processName)]
    # .sim2display <<- sim2display
    # .dt11 <<- copy(dt)
    dt <- trapro(dt, sim2display, doOrdered = TRUE)
    # .dt00 <<- copy(dt)
    
    # if foucs length > 1
    if (is.list(processGroups[[1]])){
      processGroups <- Reduce(c, processGroups)
    }
    ii <- 1
    for (ii in hseq_along(processGroups)){
      # processNames inside panel
      .www <<- processGroups[[ii]]
      processGroups[[ii]] <- trapro(processGroups[[ii]], sim2display)
      log_trace("using sim2display for processGroup {ii}: {processGroups[[ii]]}")
    }
  }
  
  
  
  if (!"panel" %in% names(dt)){
    if (!"processName" %in% names(dt)){
      stop("no processName or panel found")
    } else {
      log_trace("no panel found")
      dt[, panel := processName]
    }
  }
  droplevels.character <- function(x) {if (is.character(x)) x else droplevels(x)}
  
  # panels
  nms <- names(processGroups)
  # str(nms)
  if (length(nms)){
    nms <- trapro(nms, sim2display, doOrdered = TRUE)
  }
  # str(nms)
  ii <- 1
  for (ii in hseq_along(processGroups)){
    dt[processName %in% processGroups[[ii]]
       , panel := nms[ii] ]
  }
  dt[, panel := droplevels(panel)]
  dt[, processName := droplevels(processName)]
  
  dd <- base::split(dt, dt$panel)
  
  
  # https://stackoverflow.com/questions/41362895/r-ggplot2-change-the-spacing-between-the-legend-and-the-panel
  
  if (length(legendRows)){
    more <- c(setdiff(more, "none")
              , paste0("guides(colour = guide_legend(nrow = "
                       , legendRows
                       , ")) + theme(legend.margin = margin(-5, 0, 0, 0)"
                       , "         , legend.box.spacing = unit(-2, 'pt'))")
    )
    .more <<- more
    # element_text(lineheight=0.8), 
    # , legend.text = element_text(size=8)
  }
  
  plotList <- lapply(dd, function(x) {
    suppressMessages(
      pggs(dfg = x
           , input = input
           , facet_w = facet_w
           , doplot = FALSE
           , yois = character(0)
           , chunkTitle = NULL
           , allowppt = FALSE
           , hash = hash
           , more = more
           , ...
      ))
  }
  )
  
  .plotList <<- plotList
  if (!length(plotList)){
    log_fatal("panelPlot:| plotList is empty")
    return(ggplot2::ggplot())
  }
  plotList <- plotList[sapply(plotList, is.list)]
  p <- base::Reduce(patchworkOperator, plotList)
  if ((!is.null(panelRows)) * (!is.null(panelCols)) > 0){
    p <- p + patchwork::plot_layout(ncol = panelCols, nrow = panelRows)
  }
  if (doplot){
    print(p)
  }
  if (!is.null(chunkTitle)){
    vrmd("add", p = p, chunkId = chunkTitle)
  }
  p
}