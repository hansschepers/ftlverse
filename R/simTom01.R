#' simTom01
#' @description simulates with extrapolateYield
#' @export
simTom01 <- function(dtwClean, modelsGiven
                     , bycols = c("cycle_syn", "plot_syn")
){
  kk <- c("dateTime", bycols
          # , "light.sum.total.day"
          # , "pruning", "stem.density.setting"
  )
  dtwSun <- dtwClean[year(dateTime) == 2021 & plot_syn == "k_21_m3", ..kk]
  print(head(dtwSun))
  dtwSun[, weekno := lubridate::isoweek(dateTime)]
  if (!"light.sum.total.day" %in% names(dtwSun)){
    dtwSun[, light.sum.total.day := predict(modelsGiven$radModel
                                            , na.action = na.exclude
                                            , newdata = data.table(weekno = weekno))]
  }
  {
    sdWeek.s <- c(1, 5, 10, 15)
    sdWeek <- 15
    ii <- 0 ; res <- list()
    for (sdWeek in sdWeek.s){
      dtwSun[, stem.density.setting := ifelse(weekno <= sdWeek, 2.5, 3.75)]
      dtsim <- extrapolateYield(dtwSun, modelsGiven = modelsGiven)
      ii <- ii + 1
      res[[ii]] <- dtsim
      cat(" ") ; cat(ii)
    }
  }
  {
    dtp <- rbindlist(res, idcol = "sdWeek")
    dtp[, sdWeek := as.character(sdWeek, width = 2)]
    dtp[, afw.clean := NULL]
    dtp[, (grep("\\.clean$", names(dtp))) := NULL]
    dtp[, (grep("\\.subModel$", names(dtp))) := NULL]
    dtp[, harvestMaturityShifted := NULL]
    dtp[yield.pred == 0, afw.pred := NA]
    dtp[, zyield.cu := aphCumsum(yield.pred), by = c(bycols, "sdWeek")]
    dtpl <- aphMelt(dtp)
    p <- pggs(dtpl, foi = "sdWeek", lwd = 1.5)
    p
  }
}
