#' fit_dp new in modelReduction
#'
#' @export
fit_dp <- function(DT
                   , response = "GHTEMP"
                   , predictor = "RAD"
                   , start = list(icpt = 16
                                  , slope = 0.001  # temp_air example
                                  , delta_y = 0.5)
                   , fixedParms = list(daySwitchTime = 12
                                       , nightSwitchTime = 24
                                       , expo = 16
                                       , smoothSpanRAD = 1
                                       , rad_sat = 0
                                       , sunrise = 6
                                       , sunset = 18
                                       , autoSunRiseSet = 0)[0]
                   , doplot = FALSE
                   , tryOutside = 0
                   , align = "right"
                   , parms = list()
                   , rerunOutside = FALSE
                   , verbosity = log_threshold()
                   # , nlsControlList = nls.control(warnOnly = TRUE, maxiter = 200)
                   , ...
){
  if (nrow(DT) < 4) {
    log_error("fit_dp| not enough data")
    print(DT)
    print(fixedParms)
    return(list())
  }
  qq <- DT$doy[1]
  if(!is.null(qq)) cat(paste0(" ", qq))
  DT <- copy(DT)
  
  # str(fixedParms)
  
  DT <- cbind(DT, as.data.table(fixedParms))
  
  formu = paste0(response
                 , " ~ dayProfileModelBareParams("
                 ,"icpt, slope, delta_y"
                 # ,", fixedParms = fixedParms"  # or in DT??
                 # ,", fixedParms = get('fixedParms')"
                 ,", RAD = ", predictor
                 ,", hr = hr)")
  if (sumna(DT$GHTEMP) > 
      sumna(DT$GHTEMP.pred)){
    log_warn("fit_dp| copying GHTEMP.pred on GHTEMP because of NAs...")
    DT[, GHTEMP := GHTEMP.pred]
  }
  
  .formu <<- formu
  .DTbeforeNLS <<- copy(DT)
  if(!length(parms)){
    log_debug("fit_dp| fitting with nls...")
    fit1 <- nls(formu
                , data = DT
                , start = start
                , algorithm = "port"
                # , control = nlsControlList
                , ...
    )
    if (verbosity > 600){
      print(summary(fit1))
    }
    parms <- as.list(coef(fit1))
  }
  
  dpm <- dayProfileModel(parms
                         , fixedParms = fixedParms
                         # , smoothSpanRAD = smoothSpanRAD
                         # , rad_sat = rad_sat
                         , RAD = DT[, get(predictor)]
                         , hr = DT$hr)
  DT[, pred1 := dpm$y]
  .DT <<- copy(DT)
  
  
  DT[, resid := GHTEMP - pred1] 
  cfs1 <- as.list(coef(lm(DT$pred1 ~ DT$GHTEMP)))  
  # hcor(DT$pred1, DT$GHTEMP)
  
  DT[, pred2 := GHTEMP + tryOutside * resid]
  cfs2 <- as.list(coef(lm(DT$pred2 ~ DT$GHTEMP)))  
  # hcor(DT$pred2, DT$GHTEMP)
  
  # DT[, pred2 := pred2 + .1*(-0.5+runif(.N))]
  
  DT2 <- copy(DT)
  DT2[, GHTEMP := GHTEMP + tryOutside * resid]
  # DT2[hr %in% 14:18, GHTEMP := GHTEMP + 3]
  
  metrics1 <- list(MAPE = hMAPE(DT$GHTEMP, DT$pred1)
                   , MAE = MAE(DT$GHTEMP, DT$pred1)
                   , r2 = hcor(DT$GHTEMP, DT$pred1)^2
                   , pt_icpt = cfs1[[1]]
                   , pt_slope = cfs1[[2]])
  metrics2 <- list(MAPE = hMAPE(DT$GHTEMP, DT$pred2)
                   , MAE = MAE(DT$GHTEMP, DT$pred2)
                   , r2 = hcor(DT$GHTEMP, DT$pred2)^2
                   , pt_icpt = cfs2[[1]]
                   , pt_slope = cfs2[[2]])
  
  if (rerunOutside)  {
    fit3 <- nls(formu
                , data = DT2
                , start = start
                , algorithm = "port"
                # , control = nlsControlList
                , ...
    )
    # fit3
    # fit1
    
    parms3 <- as.list(coef(fit3))
    dpm3 <- dayProfileModel(parms3
                            , RAD = DT2[, get(predictor)]
                            , hr = DT2$hr)
    DT2[, pred3 := dpm3$y] 
    cfs3 <- as.list(coef(lm(DT2$pred3 ~ DT2$GHTEMP)))  
    .DT3 <<- copy(DT2)
    metrics3 <- list(MAPE = hMAPE(DT2$GHTEMP, DT2$pred3)
                     , MAE = MAE(DT2$GHTEMP, DT2$pred3)
                     , r2 = hcor(DT2$GHTEMP, DT2$pred3)^2
                     , pt_icpt = cfs3[[1]]
                     , pt_slope = cfs3[[2]])
  }
  res <- mget(ls())
  if (doplot){
    res$p <- plot_dp_fit(res)
  }
  res
}



#' plot_dp_fit
#' @export
plot_dp_fit <- function(dpf_env){
  
  list2env(dpf_env, environment())
  tit.oi <- paste0("doy: ", DT$doy[1])
  
  DT
  p3a <- pggs(DT, xoi = predictor, yoi = response
              , foi ="nothing", legend = "none"
              , geom = "pointline", mega = T, lwd = .1
              , label = "hr", psize = 6)
  p3a
  sosq <- sum(DT$resid^2)
  p3 <- ppggs(DT
              , p = p3a
              , xsc = c(0, NA)
              , ysc = c(15, 30)
              , foi ="nothing", legend = "none"
              , lineColor = "red"
              , title = "RTR scatter"
              , subtitle = list2title(c(parms, list(sosq = sosq)))
              , ylab = response
              , xoi = predictor, yoi = "pred1")
  if (tryOutside > 1e-5){
    p3 <- ppggs(DT
                , p = p3
                , foi ="nothing", legend = "none"
                , lineColor = "orange"
                , subtitle = list2title(parms)
                , xoi = predictor, yoi = "pred2")
    p3 <- ppggs(DT2
                , p = p3
                , foi ="nothing", legend = "none"
                , lineColor = "blue", lineAlpha = .2, lwd = 3
                , subtitle = list2title(parms)
                , xoi = predictor, yoi = "pred3")
  }
  # print(p3)
  
  # }
  # {
  DT[, tod := format(hr)]
  pggsInput <- list(xtics = 4
                    , xlab = "hour of the day"
                    , ylab = NULL
                    , foi = "nothing"
                    , xoi = "hr")
  p1 <- ppggs(DT, yoi = predictor, input = pggsInput, title = "Radiation (W/m2)"
              , subtitle = tit.oi)
  p2 <- ppggs(DT, yoi = response, input = pggsInput, title = "Temperature(hr)")
  p2 <- ppggs(DT, yoi = "pred1", p = p2
              , input = pggsInput
              , ysc = c(15, 30)
              , lineColor = "red"
              , title = "fitted Temperature(hr)"
              , subtitle = list2title(metrics1)
              )
  if (tryOutside > 1e-5){
    p2 <- ppggs(DT, yoi = "pred2", p = p2, input = pggsInput, lineColor = "orange", title = "fitted Temperature(hr)")
    p2 <- ppggs(DT2, yoi = response, p = p2, input = pggsInput, lineColor = "blue", title = "Temperature(hr)")
  }
  p4 <- ppggs(dpm, yoi = "icpt_variable", input = pggsInput, title = "Intercept(hr)")
  if (rerunOutside)  {
    p4 <- ppggs(dpm3, p = p4, lineColor = "blue", yoi = "icpt_variable"
                , input = pggsInput, title = "Intercept(hr)")
  }
  
  # p4 <- ppggs(DT, xoi = predictor, yoi = response, input = pggsInput
  #             , xtics = "auto", xlab = "Radiation (W/m2)", title = "Temperature"
  #             , label = "tod", doLabelVarColor = FALSE, mega = TRUE
  # )
  if(F){
    p5a <- pggs(DT
                , xoi = response, yoi = "pred1"
                , foi = "doy"
                , geom = "point", abline = c(0, 1)
                , ci.alpha = .2, annoFit = T)
    p5b <- pggs(DT
                , xoi = response, yoi = "pred2"
                , foi = "doy"
                , geom = "point", abline = c(0, 1)
                , ci.alpha = .2, annoFit = T)
    p5c <- pggs(DT2
                , xoi = response, yoi = "pred3"
                , foi = "doy"
                , geom = "point", abline = c(0, 1)
                , ci.alpha = .2, annoFit = T)
    p <- p1 + p2 + p3 + p4 + p5a + p5b + p5c
  } else {
    p <- p1 + p2 + p3 + p4
  }
  print(p)
  p
}
