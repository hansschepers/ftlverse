#' rangeBarPlot
#' 
#' @export
rangeBarPlot <- function(dd
                         , foi4range = "plot_syn"
                         , ...){
  dtsu <- dd[
    , hsummary(value, hfuns = "full")
    , by = c("processName", foi4range)]
  p0 <- pggs(dtsu
             , yoi = foi4range
             , foi = foi4range
             , geom = "errorhbarpoint"
             , xoi = "hmean"
             , xmin = "hquantile05", xmax = "hquantile95"
             , free_y = FALSE, free_x = TRUE
  )
  p <- pggs(dtsu
            , p = p0
            , yoi = foi4range
            , foi = foi4range
            , free_y = FALSE, free_x = TRUE
            , xmin = "hquantile25", xmax = "hquantile75"
            , geom = "errorhbar"
            , legend = "none", xlab = NULL, ylab = NULL
            , lwdEB = 4, lineAlphaEB = .2
            , ...
  )
}
