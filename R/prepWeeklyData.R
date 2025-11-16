#' prepWeeklyData
#' @export
prepWeeklyData <- function(
  DTw
  , settingCol = c("setting.fruits.m2.wk", "setting.fruits.m2")[2]
  , harvestCol = c("harvested.fruits.m2.wk", "harvested.fruits.m2")[2]
  , doi = "dateTime"
  , foi  = "plot_syn"
  , keep = c(doi, foi, "setting", "harvest"
             , "setting.truss.stem", "harvested.truss.stem"
  )
){
  dtwf <- copy(DTw)
  setnames(dtwf,   "setting.fruits.m2.wk", "setting", skip_absent=TRUE)
  setnames(dtwf, "harvested.fruits.m2.wk", "harvest", skip_absent=TRUE)
  setnames(dtwf,   "setting.fruits.m2", "setting", skip_absent=TRUE)
  setnames(dtwf, "harvested.fruits.m2", "harvest", skip_absent=TRUE)
  setnames(dtwf, settingCol, "setting", skip_absent=TRUE)
  setnames(dtwf, harvestCol, "harvest", skip_absent=TRUE)
  keep <- intersect(names(dtwf), keep)
  dtwf <- dtwf[, ..keep]
  dtwf <- dtwf[!is.na(setting)]
  dtwf[]
}