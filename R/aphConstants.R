#' aphConstants
#' 
#' @export
aphConstants <- function(){
  res <- list(xoi = "dateTime"
              , foic = "cycle_syn"
              , foip = "plot_syn"
              , foi = c("plot_syn", "cycle_syn")
              , voi = "processName"
              , doi = "dateTime"
              , seasonPhases = c(1+floor((1:51)/13), NA, NA) # 53 weeks in 4 phases
              # , value = "value"
              , keyCustomers = c("Ridge farms", "Gardeners Pride"
                                 , "Kaaij"
                                 , "Den Berk - Merksplas"
                                 , "Den Berk - Beirinckx"
                                 , "Den Berk - Vrouwkensblok"
                                 , "Den Berk - Salmmeir"
                                 , "Tomeco - Vitapower"
                                 # , "Tomeco - Tomato Masters"  # cuke
                                 , "Neegro"
                                 # , "Tomeco - VW Maxburg"  # cuke
                                 , "Kbb Holland"
                                 , "Jami")
              , yois2transfer = c("RAD", "GHTEMP", "GHCO2C", "ECIRRI")
              , yoisWeek = c("yield", "yield.plot"
                             , "truss.rate.fromData"
                             , "truss.rate.flowering", "truss.rate.setting"
                             , "harvested.fruits.m2.wk", "setting.fruits.m2.wk"
                             , "pruning"
                             , "stem.density.setting"
                             , "stem.density.harvest"
                             , "afw"
                             , "temp.12hours", "temp24hr"
                             , "growth.days"
                             , "flowering.height"
                             , "flowering.truss.cu"
                             , "light.sum.total.day"
                             , "lightsum.day.years.out"
                             , "Production.Area.Priva...kg.m2."
                             # , "lightsum.day.out"
                             # , "avg.temp.9wk"
                             , "co2.level.day"
                             , "water.m2.day"
                             , "harvested.truss.stem", "setting.truss.stem"
                             , "plantload.fruits.m2"
                             , "plantload.fruits.m2.calc"
                             , "flowering.height"
                             , "growth.weekdata"
                             , "leaf.length"
                             , "leaf.plant"
                             , "stem.diam"
              )
              , yoisIoT = c("GHTEMP", "OUTEMP", "GHCO2C", "ECIRRI", "VADEFE"
                            , "RAD", "Irrigation"
                            , "growthRate", "plant.weight.m2")
              , aphExtentions = c("\\.subModel$","\\.predPooled$", 
                                  "\\.actual$","\\.ml$", 
                                  "\\.pred$","\\.clean$",
                                  "\\.used$","\\.filled$",
                                  "\\.mix$","\\.check$", 
                                  "\\.shifted$", "\\.lzman$"
                                  )
  )
  res$yoisNice2Have = c(res$yoisWeek, res$yois2transfer)
  res
}