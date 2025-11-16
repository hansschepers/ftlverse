#' ribbonMinMaxer
#' helper function for ribbonExporter
#' @export
ribbonMinMaxer <- function(dt){
  dt[, hmax := pmax(0, hmax)]
  dt[, hmin := pmax(0, hmin)]
  # dt[hmax < hmin, hmax := hmin]

  # dt[grep("\\.cu$", processName), hmax := keepCumulativeIncreasing(hmax), by = processName]
  # dt[grep("\\.cu$", processName), hmin := keepCumulativeIncreasing(hmin), by = processName]
  dt[hmax < hmin, hmax := hmin]

  dt[processName %in% c("settingSuccess"), hmin := pmin(1, hmin)]
  dt[processName %in% c("settingSuccess"), hmax := pmin(1, hmax)]
  dt[processName %in% c("settingSuccess"), hmin := pmax(0, hmin)]
  dt[processName %in% c("settingSuccess"), hmax := pmax(0, hmax)]
  dt[processName %in% c("harvestMaturity"), hmax := pmin(99, hmax)]
  dt[processName %in% c("brix_slow"), hmin := pmax(2, hmin)]
}



#' ribbonExporter
#'
#' @export
ribbonExporter <- function(ribbonList
                           , proj_id = ribbonList$SIMS$proj_id
                           , ribbonId
                           , focus = c("pb_driv", "pb_crop")
                           , ribbonYois = aphKpis(focus)
                           , degree = 6
                           # , traTable = c("sim2displayEN", "sim2data")[2]
                           # , sim2data = character(0)
                           , noSmoothYois = c("stemDensity.setting", "pruning", "LAI"
                                              # , "outside_temp", "cloudiness"
                                              )
                           ){
  stopifnot(all.equal(ribbonList$SIMS$proj_id, proj_id))

  # if (!length(sim2data)){
  #   sim2data <- readKB_LIST()$sim2data
  # }

  # compute and optionally export _weekly_ ribbon
  # ribbonList$dt_ribbons[is.na(hmin)]
  # ribbonList$dt_ribbons[, hsummary(hmax)]
  aphVariableLevels(ribbonList$dt_ribbons)
  {
    dt_ribbons.sm <- smoothRibbon(dt = ribbonList$dt_ribbons
                                 , degree = degree
                                 , noSmoothYois = noSmoothYois
                                 , xoi = "local_time")
    # by reference!
    .dtMiMa1 <<- copy(dt_ribbons.sm)
    ribbonMinMaxer(dt_ribbons.sm)

    if (! "all" %in% ribbonYois){
      DT_exportedRibbon_w <- dt_ribbons.sm[processName %in% ribbonYois]
    } else {
      DT_exportedRibbon_w <- copy(dt_ribbons.sm)
    }

    DT_exportedRibbon_w2 <- copy(DT_exportedRibbon_w)
    # DT_exportedRibbon_w2[, processName := trapro(processName, traTable)]
    DT_exportedRibbon_w2[, wk := lubridate::isoweek(local_time)]
    DT_exportedRibbon_w2[, DAP := as.numeric(difftime(local_time, min(local_time), units = "days"))]
    DT_exportedRibbon_w2 <- addWAP(DT_exportedRibbon_w2)
    col2remove <- c("plot_syn", "status", "modelId", "local_time", "local_timeNum")
    col2remove <- intersect(col2remove, names(DT_exportedRibbon_w2))
    DT_exportedRibbon_w2[, (col2remove) := NULL]
  }

  .DT_exportedRibbon_w2 <<- DT_exportedRibbon_w2

  # compute and optionally export _daily_ ribbon
  {
    timeFiller <- data.table(Time = 0:350)
    timeFiller[, WAP := floor(Time / 7)]# + 1
    DT_exportedRibbon_d <- merge(timeFiller
                                 , DT_exportedRibbon_w2
                                 , by = "WAP"
                                 , all.x = TRUE#, all.y = TRUE
                                 , allow.cartesian = TRUE)
    aphKey(DT_exportedRibbon_d)
    setcolorder(DT_exportedRibbon_d)
    DT_exportedRibbon_d <- smoothRibbon(dt = copy(DT_exportedRibbon_d)
                                        , degree = degree
                                        , xoi = "Time")
    # by reference!
    .dtMiMa2 <<- copy(DT_exportedRibbon_d)
    ribbonMinMaxer(DT_exportedRibbon_d)

    DT_exportedRibbon_d[, WAP := NULL]
    DT_exportedRibbon_d[, DAP := NULL]
    setnames(DT_exportedRibbon_d, "Time", "DAP")
  }
  guardrails <- makeMinMaxFromExportedRibbonsHS(
     DT_exportedRibbon_d = DT_exportedRibbon_d
    , DT_exportedRibbon_w2 = DT_exportedRibbon_w2
  )
  mget(setdiff(ls(), "ribbonList"))
}
