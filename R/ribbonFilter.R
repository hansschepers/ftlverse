#' ribbonFilter
#' @examples \dontrun{
#'   ribbonListFiltered <- ribbonFilter(ribbonList, filters = list(minYieldQuantile = .7), omitvaryList = TRUE)
#' }
#' @export
ribbonFilter <- function(ribbonList = list()
                         , df_kpiLinfo = ribbonList$df_kpiLinfo
                         , filters = ribbonList$ribbonSpecs$filters
                         , omitvaryList = FALSE
){
  if (is.null(filters)){
    filters <- makeFilter()
  }
  {
    stopifnot("yield" %in% unique(df_kpiLinfo$processName))
    # stopifnot("minYieldQuantile" %in% names(filters))

    # sum-yield
    df_kpiOK <- df_kpiLinfo[processName == "yield" & kpi == "hsum"]
    maxSumYieldRaw <- df_kpiOK[value == hmax(value)]
    # maxSumYieldRaw

    df_kpiOK <- df_kpiOK[value > 0]

    if (!is.null(filters$minimumYield)) {
      minimumYield <- filters$minimumYield
      df_kpiOK <- df_kpiOK[value >= minimumYield]
    }
    if ("minYieldQuantile" %in% names(filters)){
      minimumYield <- df_kpiOK[, quantile(value, filters$minYieldQuantile)]
      df_kpiOK <- df_kpiOK[value >= minimumYield]
    }

    OkIds <- df_kpiOK$scenId
  }



  if (!length(ribbonList)){
    log_debug("returning OkIds!")
    return(OkIds)
  } else {
    log_debug("returning full re-filtered ribbonList!")
    ribbonList$filters <- filters
    ribbonList$OkIds <- OkIds
    ribbonList$minimumYield <- minimumYield
    ribbonList$maxSumYieldRaw <- copy(maxSumYieldRaw)

    ribbonList$d_p <- rbindlist(lapply(ribbonList$varyList$scenList[ribbonList$OkIds]
                                       , getElement, "cropLong")
                                , idcol = "scenId")
    # Aggregate
    ribbonList$dt_ribbons <- aphAggregate(ribbonList$d_p
                                          , accross = "scenId"
                                          , expr = "ribbon")
    if (omitvaryList){
      ribbonList$varyList <- NULL
      strList(ribbonList)
    }
    return(ribbonList)
  }
}
