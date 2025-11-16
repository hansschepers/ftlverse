#' fit_dp2 new in modelReduction
#' @examples \dontrun{
#'   DT <- dayProfileModel(parms = list(delta_y = 9))
#'   attributes(DT)
#' }
#'
#' @export
fit_dp2 <- function(DT
                    , tempName = "GHTEMP"
                    , start = list(icpt = 16
                                   , slope = 0.01  # temp_air example
                                   , delta_y = 4)
                    , doplot = TRUE
){
  if (nrow(DT) < 4) {
    log_error("fit_dp2|not enough data")
    return(list())
  }
  Pars <- list(icpt = 16
               , slope = 0.01  # temp_air example
               , delta_y = 4
               , daySwitchTime = 12
               , nightSwitchTime = 24
               , expo = 16)
  for (pp in setdiff(names(Pars), names(start))){
    DT[, (pp) := Pars[[pp]]]
  }
  # dayProfileModel(parms = start, x = DT$RADJCM, hr = DT$hr)$y
  # dayProfileModelBareParams(15, .001, 4, x = DT$RADJCM, hr = DT$hr)
  formu = "GHTEMP ~ dayProfileModelBareParams(icpt, slope, delta_y
          , x = RADJCM, hr = hr)"
  fit <- nls(formu
             , data = DT
             , start = start
             , algorithm = "port"
  )
  summary(fit)
  parms <- as.list(coef(fit))
  dpm <- dayProfileModel(parms, x = DT$RADJCM, hr = DT$hr)
  DT[, pred := dpm$y] #dayProfileModel(parms = parms, x = DT$RADJCM, hr = DT$hr)$y]
  DT[, resid := GHTEMP - pred] #dayProfileModel(parms = parms, x = DT$RADJCM, hr = DT$hr)$y]
  metrics <- list(MAPE = hMAPE(DT$GHTEMP, DT$pred)
                  , MAE = MAE(DT$GHTEMP, DT$pred))
  if (doplot){
    DT
    p0 <- pggs(DT, xoi = "RADJCM", yoi = "GHTEMP"
               , foi ="nothing", legend = "none"
               , geom = "pointline", mega = T, lwd = .1
               , label = "hr", psize = 6)
    p0
    p3 <- ppggs(DT
                , p = p0
                , foi ="nothing", legend = "none"
                , lineColor = "red"
                , subtitle = list2title(parms)
                , xoi = "RADJCM", yoi = "pred")
    # print(p3)
    
    # }
    # {
    DT[, tod := format(hr)]
    pggsInput <- list(xtics = 6
                      , xlab = "hour of the day"
                      , ylab = NULL
                      , foi = "nothing"
                      , xoi = "hr")
    p1 <- ppggs(DT, yoi = "RADJCM", input = pggsInput, title = "Radiation (W/m2)")
    p2 <- ppggs(DT, yoi = "GHTEMP", input = pggsInput, title = "Temperature(hr)")
    p2 <- ppggs(DT, yoi = "pred", p = p2, input = pggsInput, lineColor = "red", title = "fitted Temperature(hr)")
    
    p4 <- ppggs(dpm, yoi = "icpt_variable", input = pggsInput, title = "Intercept(hr)")
    
    # p4 <- ppggs(DT, xoi = "RADJCM", yoi = "GHTEMP", input = pggsInput
    #             , xtics = "auto", xlab = "Radiation (W/m2)", title = "Temperature"
    #             , label = "tod", doLabelVarColor = FALSE, mega = TRUE
    # )
    p <- p1 + p2 + p3 + p4
    print(p)
  }
  list(parms = parms
       , metrics = metrics
       , DT = DT
       , formu = formu
       , start = start)
}
