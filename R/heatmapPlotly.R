#' heatmapPlotly
#' @rawNamespace import(plotly, except = c(last_plot, config))
#' @export
heatmapPlotly <- function(DT
                          , xoi = "doy", xlab = xoi
                          , yoi = "hr", ylab = yoi
                          , zoi = "value"
                          , tit = ""
                          , colors = colorRamp(c("yellow","red"))
                          ){
  DT <- copy(DT)
  setnames(DT, xoi, "xoi")
  setnames(DT, yoi, "yoi")
  setnames(DT, zoi, "zoi")
  p <- plotly::plot_ly(
    DT, 
    x = ~xoi,
    y = ~yoi,
    z = ~zoi,
    type = "heatmap",
    colors = colors
  )
  
  p <- plotly::layout(
    p, 
    title = tit,
    xaxis = list(
      title = xlab,
      zeroline = TRUE
    ),
    yaxis = list(
      title = ylab,
      showlegend = FALSE
    )
  )
  p
}
