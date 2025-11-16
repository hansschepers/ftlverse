#' prepClean
#' @examples \dontrun{
#' }
#' @export
prepClean <- function(
  DT
  , bycols = intersect(names(DT), c("plot_syn", "cycle_syn")) # setdiff(aphFactors(DT), aphVariables(DT)) #
  , correctMissingYield = FALSE
  , segment = "large"
  , prepPars = getPrepPars(segment)
  , voi = "processName"
  , doi = "dateTime"
  , addWeekClusters = c("yield", "harvest")[0]
  , verbosity = log_threshold()
  # , ...
){
  # dots <- list(...)
  ############################################################## CAST!  
  keep <- setdiff(names(DT), c("weekday", "hr", "wk", "weekno", "mon", "year"))
  keep <- c(bycols, voi, doi, "value")
  DT <- DT[, ..keep]
  dtw <- hdcast(DT)
  aphKey(dtw)
  skip_absent <- FALSE
  setnames(dtw,   "setting.fruits.m2.wk", "setting", skip_absent = skip_absent)
  setnames(dtw, "harvested.fruits.m2.wk", "harvest", skip_absent = skip_absent)
  if ("temp.12hours" %in% names(dtw)){
    setnames(dtw, "temp.12hours", "temp24hr", skip_absent = FALSE)
  }
  if ("GHTEMP" %in% names(dtw)){
    setnames(dtw, "GHTEMP", "temp24hr", skip_absent = FALSE)
  }
  if ("RAD" %in% names(dtw)){
    dtw[, RAD := universalConstants()$J2W * RAD]
    setnames(dtw, "RAD", "light.sum.total.day", skip_absent = FALSE)
  }
  
  dtw[is.na(harvest) | harvest == 0 | afw < 5, afw := NA]
  
  # dtw <- rbindlist(lapply(split(dtw, by = c(bycols)), function(x) {if (okData(x$pruning) == 0) x$pruning <- prepPars$basePruning ; x}))
  # dtw <- rbindlist(lapply(split(dtw, by = c(bycols)), function(x) {if (okData(x$stem.density.setting) == 0) x$stem.density.setting <- 2.5 ; x}))
  # dtw[okData(pruning) == 0, pruning := prepPars$basePruning, by = c(bycols)]
  if (verbosity > 400){
    print(dtw[, hsummary(pruning), by = c(bycols)])
    print(dtw[, hsummary(stem.density.setting), by = c(bycols)])
  }
  
  if (!"pruning" %in% names(dtw)){
    dtw[, pruning := prepPars$basePruning]
  }
  dtw[pruning < 0, pruning := prepPars$basePruning * 1.1]
  dtw[pruning == 0, pruning := prepPars$basePruning]
  
  dtw[, setting := findTails(setting, which = "left", repl = 0), by = c(bycols)]
  dtw[, harvest := findTails(harvest, which = "left", repl = 0), by = c(bycols)]
  dtw[, setting := fillInternalNAs(setting), by = c(bycols)]
  dtw[, harvest := fillInternalNAs(harvest), by = c(bycols)]  #TODO 
  
  # truss speed from setting, QA'd / interpolated, then setting computed from trussSpeed again!
  {
    # if ("setting.truss.stem" %in% names(dtw)){
    #   dtw[, trussSpeed = diff1(setting.truss.stem), by = c(bycols)]
    # } else {
    # trusses from truss, pruning, stem density
    # top-down:
    dtw[, pruning              := aphApprox2(pruning),              by = c(bycols)]
    dtw[, stem.density.setting := aphApprox2(stem.density.setting), by = c(bycols)]
    
    dtw[, trussSpeed := fillInternalNAs(setting) / pruning / stem.density.setting, by = c(bycols)]
    dtw[trussSpeed <= prepPars$minTrussSpeed |
          trussSpeed >= prepPars$maxTrussSpeed, trussSpeed := NA]
    dtw[, setting := trussSpeed * pruning * stem.density.setting, by = c(bycols)]
  }
  
  initHanging <- 0  # getEnrichPars()$initHanging
  dtw[, setting.cum := aphCumsum(setting) + initHanging, by = c(bycols)]
  dtw[, harvest.cum := aphCumsum(harvest)              , by = c(bycols)]
  
  dtw[, harvestMaturity := calcMaturity(setting.cum
                                        , harvest.cum
                                        , isCumulative = TRUE
                                        , n = 1
                                        , keepOnMax = TRUE)
      , by = c(bycols)]
  
  dtw[, plantLoad := setting.cum - harvest.cum, by = c(bycols)] # - aphCumsumaborted))
  dtw[plantLoad < 0, plantLoad := 0]
  
  # correct missing Yield (if afw and harvest are present)
  # because of cast there are NA's, which can be helpful
  if (correctMissingYield){
    dddd <- dtw[((!is.na(harvest)) & (!is.na(afw)) & (is.na(yield)))]
    if (nrow(dddd) > 0){
      log_trace("data without yield, but with AFW and harvest {dddd$dateTime}")
    }
    dtw[((!is.na(harvest)) & (!is.na(afw)) & (is.na(yield)))
        , yield := afw * harvest / 1000]
  }
  
  for (var in addWeekClusters){
    dtw[, `:=`(paste0(var, ".cum"), aphCumsum(get(var))), by = c(bycols)]
    dtw[, `:=`(paste0(var, ".by2"), hfrollmean(get(var), n = 2, align = "right")), by = c(bycols)]
    dtw[, `:=`(paste0(var, ".by3"), hfrollmean(get(var), n = 3, align = "right")), by = c(bycols)]
  }
  
  aphKey(dtw, ignoreAsDois = "wk")
  return(dtw[])
}
