#' dpmPlot
#' 
#' @export
dpmPlot <- function(dpm
                    , pggsInput = list(xtics = 6
                                       , xlab = "hour of the day"
                                       , ylab = NULL
                                       , foi = "nothing"
                                       , xoi = "hr"
                                       , subtitleSize = 9)
                    , response = "GHTEMP"
                    , predictor = "RAD"
                    , doplot = FALSE
                    , ...){
  dots <- list(...)
  pggsInput <- mergeParameters(pggsInput, dots)
  p1 <- ppggs(dpm
              , yoi = predictor
              , input = pggsInput
              , title = "Radiation (W/m2)")
  p2 <- ppggs(dpm
              , yoi = response
              , input = pggsInput
              , title = "Temperature(hr)"
              , ytics = 1)
  p3 <- ppggs(dpm
              , yoi = "icpt_variable"
              , input = pggsInput
              , title = "Intercept(hr)"
              , subtitle = list2title(parmsDemo))
  p4 <- ppggs(dpm
              , xoi = predictor
              , yoi = response
              , input = pggsInput
              , xtics = "auto"
              , xlab = "Radiation (W/m2)"
              , title = "Temperature"
              , label = "hr"
              , doLabelVarColor = FALSE
              , mega = TRUE)
  p <- p1 + p2 + p3 + p4
  if (doplot) print(p)
  p
}