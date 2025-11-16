#' aphHeatmap
#' @examples \dontrun{
#'   library(data.table)
#'   dt <- CJ(x = -5:5, y = 2:5)
#'   dt[, z := x^2 + y*x]
#'   aphHeatmap(dt, xoi = "x", yoi = "y", z = "z")
#' }
# @rawNamespace import(plotly, except = c(last_plot, config))
#' @export
aphHeatmap <- function(dt, xoi, yoi, zoi = "N"){
  dt <- as.data.table(dt)
  dt <- copy(dt)
  setnames(dt, xoi, "x")
  setnames(dt, yoi, "y")
  setnames(dt, zoi, "z")
  plotly::plot_ly(dt
                  , x = ~x
                  , y = ~y
                  , z = ~z
                  , type = "heatmap"
                  # , colors = colorRamp(c("blue", "yellow"))
  )
}