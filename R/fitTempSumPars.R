#' fitTempSumPars
#'
#' @export
fitTempSumPars <- function(dtw
                        , moi = "harvestMaturity"
                        , tempName = "temp24hr.clean"
                        , formu = as.formula(paste0(moi, "~ M/7 / (",tempName," + ",tempExtra," - Tb)"))
                        , start = list(M = 1000, Tb = 5)
                        , tempExtra = 0
){
  fit3 <- nls(formu
              , data = dtw#[!is.na(get(moi))]
              , na.action = na.exclude
              , start = start
              , algorithm = "port")
  summary(fit3)
  ab3 <- coef(fit3)
  Tb3 <-  ab3[2]
  M3 <- ab3[1]
  return(list(maturityDegreeDays = M3, baseTemp = Tb3))
}



#' fitMaturityParameters
# @export
# fitMaturityParameters <- function(DT
#                                   , moi = "harvestMaturity"
#                                   , doPlot = FALSE)
# {
#   formu = as.formula("harvestMaturity ~ M/7 / (Tavg - Tb)")
#   
#   fit2p <- nls(formu
#                , data = DT[!is.na(get(moi))]
#                , start = list(M = 1000, Tb = 5)
#                , algorithm = "port")
#   summary(fit2p)
#   ab3 <- coef(fit2p)
#   
#   Tb3 <-  ab3[2]
#   M3 <- ab3[1]
#   DT <- DT[!is.na(get(moi))]
#   DT$pred <- predict(fit2p)
#   cor(DT$pred, DT$harvestMaturity)
#   
#   DT[, canCloWeek := 19]
#   DT[, tempExtra := 1]
#   formu4p = as.formula("harvestMaturity ~ M/7 / (Tavg + (weekno < canCloWeek)*tempExtra - Tb)")
#   fit2pcc <- nls(formu4p
#                  , data = DT[!is.na(get(moi))]
#                  , start = list(M = 1000, Tb = 5)
#                  , algorithm = "port")
#   coef(fit2pcc)
#   DT$pred2pcc <- predict(fit2pcc)
#   cor(DT$pred2pcc, DT$harvestMaturity)
#   
#   DT[, tempExtra := NULL]
#   fit3pcc <- nls(formu4p
#                  , data = DT[!is.na(get(moi))]
#                  , start = list(M = 1000, Tb = 5, tempExtra = 1)
#                  , algorithm = "port")
#   coef(fit3pcc)
#   DT$pred3pcc <- predict(fit3pcc)
#   cor(DT$pred3pcc, DT$harvestMaturity)
#   
#   DT[, weekGroup := ifelse(weekno < 30, "early", "late")]
#   if (doPlot){
#     p2p <- pggs(DT, xoi = "Tavg", yoi = moi
#                 , foi = "weekGroup"
#                 # , facet_w = foi
#                 , label = "weekno", labelSize = 5
#                 # , input = input
#                 , doplot = FALSE
#                 , psize = 4
#                 , geom = "pointline", lwd = .1)
#     p2pcc <- pggs(DT, xoi = "Tavg", yoi = "pred2pcc"
#                   , foi = "weekGroup"
#                   # , facet_w = foi
#                   # , label = "weekno", labelSize = 4
#                   , p = p2p
#                   # , input = input
#                   # , psize = 4
#                   , geom = "pointline", lwd = 1, lineColor = "green")
#     
#     p3pcc <- pggs(DT, xoi = "Tavg", yoi = "pred3pcc"
#                   , foi = "weekGroup"
#                   # , facet_w = foi
#                   # , label = "weekno", labelSize = 4
#                   , p = p2pcc
#                   # , input = input
#                   # , psize = 4
#                   , geom = "pointline", lwd = 3, lineColor = "blue")
#   }
#   
#   # (weekno < canCloWeek)*tempExtra + 
#   formu6p = as.formula("harvestMaturity ~ M/7 / (Tavg + (abs(weekno - openCropTop) < 2) * openCropExtent - Tb)")
#   formu6p = as.formula("harvestMaturity ~ M/7 / (Tavg + openCropExtent / ((weekno - openCropTop)^2)  - Tb)")
#   DT[, tempExtra := .8]
#   fit6p <- nls(formu6p
#                , data = DT[!is.na(get(moi))]
#                , start = list(M = 1100, Tb = 5, openCropTop = 24, openCropExtent = .1)
#                # , lower = list(M = 800, Tb = 4, openCropTop = 23, openCropExtent = .9)
#                # , upper = list(M = 1400, Tb = 15, openCropTop = 25, openCropExtent = 1.1)
#                , algorithm = "port")
#   coef(fit6p)
#   DT$pred6p <- predict(fit6p)
#   cor(DT$pred6p, DT$harvestMaturity)
#   
#   
#   if (doPlot){
#     p6p <- pggs(DT, xoi = "Tavg", yoi = "pred6p"
#                 , foi = "weekGroup"
#                 , p = p2p
#                 , geom = "pointline", lwd = 3, lineColor = "blue")
#   }
#   
#   
#   # (weekno < canCloWeek)*tempExtra + 
#   formu7p = as.formula("harvestMaturity ~ M/7 / (Tavg + (weekno < canCloWeek)*tempExtra + 
#                      openCropExtent / ((weekno - openCropDuration*openCropTop)^2)  - Tb)")
#   DT[, tempExtra := NULL]
#   DT[, openCropDuration := 1]
#   # DT[, openCropDuration := NULL]
#   fit7p <- nls(formu7p
#                , data = DT[!is.na(get(moi))]
#                , start = list(M = 900, Tb = 7.2
#                               # , canCloWeek = 19
#                               , tempExtra = 0.75
#                               # , openCropDuration = .5
#                               , openCropTop = 25, openCropExtent = 0.2)
#                # , lower = list(M = 600, Tb = 3, openCropTop = 23, openCropExtent = 0.01
#                #                , tempExtra = 0.15
#                #                # , openCropDuration = 0
#                # )
#                # , upper = list(M = 1100, Tb = 11, openCropTop = 26, openCropExtent = 0.83
#                #                , tempExtra = 0.95
#                #                # , openCropDuration = 2
#                # )
#                , algorithm = "port")
#   summary(fit7p)
#   DT$pred7p <- predict(fit7p)
#   cor(DT$pred7p, DT$harvestMaturity)
#   
#   
#   if (doPlot){
#     p7p <- pggs(DT, xoi = "Tavg", yoi = "pred7p"
#                 , foi = "weekGroup", fsize = 16
#                 , ylab = "Harvest Maturity (Weeks)"
#                 , xlab = "Average Temperature"
#                 , p = p2p
#                 , geom = "pointline", lwd = 3, lineColor = "blue"
#                 , chunkTitle = "HarvestMaturity vs Temperature, with openCrop episode around week 24 and unclosed Canopy before week 19")
#   }
#   list(fit2p = fit2p, fit6p=fit6p, fit7p = fit7p)
# }
# 
