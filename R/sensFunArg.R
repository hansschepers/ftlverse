#' sensFunArg
#' @export
sensFunArg <- function(DT
                       , ranges
                       , steps = 5
                       , FUN
                       , targetArg = "curvy"
){
  varyPlan <- makescenPlan(ranges = ranges, steps = steps)
  varyPlan
  scenDT <- plan2scenDT(varyPlan, doAddBase = FALSE)
  scenDT
  resList <- aphVary(scenDT
                     , FUN = FUN
                     , funArgs = list(DT = DT)
                     , targetArg = targetArg
                     , targetArgIsList = FALSE)
  resList <<- resList
  dt_sensFunArg <- rbindlist(resList, idcol = "scenId")
  dt_sensFunArg <- scenDT[dt_sensFunArg, on = "scenId"]
  dt_sensFunArg
}


#' sensFunArg2
#' @export
sensFunArg2 <- function(FUN
                        , targetArg
                        , funArgs = list()
                       , ranges
                       , steps = 5
                       , type = c("ribs", "star", "sides", "crossjoin")[4]
                       , targetArgIsList = FALSE){
  varyPlan <- makescenPlan(ranges = ranges, steps = steps)
  varyPlan
  scenDT <- plan2scenDT(varyPlan
                        , doAddBase = FALSE
                        , type = type)
  scenDT
  resList <- aphVary(scenDT
                     , FUN = FUN
                     , funArgs = funArgs
                     , targetArg = targetArg
                     , targetArgIsList = targetArgIsList)
  resList <<- resList
  resList <- lapply(resList, as.data.table)
  dt_sensFunArg <- rbindlist(resList, idcol = "scenId")
  dt_sensFunArg <- scenDT[dt_sensFunArg, on = "scenId"]
  dt_sensFunArg
}
