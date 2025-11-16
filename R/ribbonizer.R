#' ribbonizer
#' @export
ribbonizer <- function(
    SIMS
    , ribbonSpecs
    # , locationWeather = list(historic = data.table())
    # , focus = c("crop", "driv")  # unused
    , proj_id = SIMS$proj_id
    , proj_nr = substr(proj_id, 1, 3)
    # , segment.oi = SIMS$segment.oi
    , daysPerStep = SIMS$daysPerStep
    , doFilter = FALSE
    , FUN = runFun
    , runAgain = TRUE
    , ...
){

  #   planSpecs_Out <- SIMS
  #   SIMS <- runFun(input = SIMS)
  # }

  if (!runAgain){
    exists(".varyList")
    if (exists(".varyList")){
      varyList <- .varyList
      scenDT <- varyList$scenDT
    } else {
      log_info("ribbonizer| no prior .varyList found putting runAgain = TRUE")
      runAgain <- TRUE
    }
  }

  # this block could go into ribbonizer
  {
    ignoreEvents <- setdiff(names(ribbonSpecs$scenDT), "scenId")
    if ("pruningDefault" %in% ignoreEvents){
      ignoreEvents <- union(ignoreEvents, "pruning")
    }
    if ("stemDensityStart" %in% ignoreEvents){
      ignoreEvents <- union(ignoreEvents, "stemDensity.setting")
    }
    ignoreEvents <- intersect(names(SIMS$eventList), ignoreEvents )
    log_info("taking out from event-Lists: {paste(ignoreEvents, collapse = ',')}")
  }
  log_debug("ignoreEvents: {ignoreEvents}")
  if (length(ignoreEvents)) {
    message("ignoreEvents: ", ignoreEvents)
  }


  if (runAgain){
    varyList <- aphScenarios(ribbonSpecs$scenDT
                             , SIMS = SIMS
                             # , locationWeather = locationWeather
                             # , drivers_weekData = drivers_weekData
                             , ignoreEvents = ignoreEvents
                             , FUN = FUN
                             , daysPerStep = daysPerStep
                             , ...)
    .varyList <<- varyList
    # strList(.varyList[[1]])
  }
  # varyList <- .varyList
  # fillScenDT(scenDT, varyList$SIMS$usedParms)
  varyList$scenDT_unfilled <- copy(varyList$scenDT)
  varyList$scenDT <- fillScenDT(varyList$scenDT, varyList$SIMS$usedParms)
  varyList$scenDT

  df_kpiL <- simsSummary(varyList$scenList
                         , yois = aphKpis()  # wider than focus..
                         , doPretty = FALSE
                         , doFull = TRUE)
  df_kpiL
  df_kpiLinfo <- varyList$scenDT[df_kpiL, on = "scenId"]
  dt_kpiRanges <- df_kpiLinfo[, hsummary(value, "ribbon")
                              , by = .(processName, kpi)]

  # criteria, filter
  if (!length(ribbonSpecs$filters)) doFilter <- FALSE
  if (doFilter){
    OkIds <- ribbonFilter(df_kpiLinfo = df_kpiLinfo
                          , filters = ribbonSpecs$filters)
  } else {
    OkIds <- seq_along(varyList$scenList)
  }

  d_p <- rbindlist(lapply(varyList$scenList[OkIds]
                          , getElement, "cropLong")
                   , idcol = "scenId")
  # Aggregate
  # message("****************************** 89 ")
  # str(d_p)
  .d_p <<- copy(d_p)
  # d_p <<- copy(.d_p)
  dt_ribbons <- aphAggregate(d_p
                             , accross = "scenId"
                             , expr = "ribbon")
  # rm(d_p)
  ribbonList <- mget(ls())
  ribbonList
}
