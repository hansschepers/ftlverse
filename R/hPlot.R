#' S3 generic to plot
#' @export
hPlot <- function( ...) {
  UseMethod("hPlot")
}

#' hPlot.default
#' 
#' for data.frame, tibble, data.tables
#' 
#' @examples \dontrun{
#'   ToothGrowth <- datasets::ToothGrowth
#'   hPlot(ToothGrowth, engine = "plotly")
#'   hPlot(ToothGrowth, engine = "plotly", geom="bar")
#'   hPlot(ToothGrowth, engine = "pggs")
#' }
#' @rawNamespace import(plotly, except = c(last_plot, config))
#' @export
hPlot.default <- function(dfg = ToothGrowth
                            , xoi = "dose"
                            , yoi = "len"
                            , foi = "supp"
                            , title = NULL
                            , xlab = xoi
                            , ylab = yoi
                            , geom = "line"
                            , engine = c("plotly", "pggs", "easyGgplot2")[2]
                            , ...
){
  dfg <- copy(as.data.table(dfg))
  # setnames(dfg, )
  
  if (engine == "pggs"){
    if (is.null(title)) title = "none"
    p0 <- pggs(dfg = dfg
               , xoi = xoi, yoi = yoi, foi = foi
               , title = title, xlab = xlab, ylab = ylab
               , geom = geom
               , doplot = FALSE
               , ...)
  }
  
  
  if (engine == "plotly"){
    typePlotly <- "scatter"
    if (grepl("bar", geom)) typePlotly <- "bar"
    modePlotly <- "markers"
    if (grepl("line", geom)) modePlotly <- "lines"
    argsPlotly <- list(
      data = dfg
      , type = typePlotly
      , x = ~get(xoi)
      , y = ~get(yoi)
      , split = ~get(foi)
    )
    if (typePlotly != "bar") argsPlotly$mode <- modePlotly
    argsPlot <- c(argsPlotly, list(...))
    p0 <- do.call(plotly::plot_ly, argsPlotly)
  }
  
  
  # if (engine == "easyGgplot2"){
  #   if (geom == "boxplot"){
  #     # NOTE: easyGgplot2 is not on CRAN. So it cannot be deployed that easily.
  #     p0 <-easyGgplot2::ggplot2.boxplot(data = dfg
  #                                       , xName = xoi, yName = yoi
  #                                       , groupName = foi
  #                                       , xtitle = xlab, ytitle = ylab
  #                                       , mainTitle = title
  #                                       , ...
  #     )
  #   }
  # }
  
  return(p0)
}




#' hPlot.SIMS
#' @examples \dontrun{
#' SIMS <- modelReduction::runFun(modelId = "x01Pde"
#'     , auxParms = list(maxHarvestRate = 0.15)
#'     , timesSim = 5 * 1:70)
#'   names(SIMS)
#'   SIMS$defaultParms$maxHarvestRate
#'   
#'   p <- hPlot(SIMS, yGroupPatterns = c("Xa" = "FruitAgeClasses"))
#'   p
#' }
#' @export
hPlot.SIMS <- function(SIMS
                       , yGroups
                       , yGroupPatterns
                       , dt_oi = "results"
                       , legend = "none"
                       , ...){
  dtlong <- aphMelt(SIMS[[dt_oi]])
  
  if (missing(yGroups)){
    if(exists("yGroups", envir = .GlobalEnv)){
      message("yGroups missing, taking from .GlobalEnv")
      yGroups <- get("yGroups", envir = .GlobalEnv)
    } else {
      yGroups <- list()
    }
  }
  if (missing(yGroupPatterns)){
    if(exists("yGroupPatterns", envir = .GlobalEnv)){
      message("yGroupPatterns missing, taking from .GlobalEnv")
      yGroupPatterns <- get("yGroupPatterns", envir = .GlobalEnv)
    } else {
      yGroupPatterns <- list()
    }
  }
  
  tmpLong <- copy(dtlong)
  tmpLong <- addYgroups(tmpLong
                                 , pattern = yGroupPatterns
                                 , yGroups = yGroups)
  aphKey(tmpLong)
  .tmpLong <<- copy(tmpLong)
  pggs(tmpLong, legend = legend, ...)
}
