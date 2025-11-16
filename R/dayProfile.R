#' dayProfile
#' @export
dayProfile <- function(dt0_h
                       , yoi = aphVariableLevels(dt0_h)
                       , foi = "cropseason_id"
                       , facet_w = "processName"
                       , geom = c("ribbonline","ribbonlinepoint")[2]
                       , xlab = "hour of the day"
                       , ribbon_sds = numeric(0)
                       , input = list()
                       , addTraces = FALSE
                       , ...
){
  if (!any(yoi %in% aphVariableLevels(dt0_h))) return(ggplot2::waiver())
  if(!"processName" %in% names(dt0_h)){
    log_info("melting...")
    dt0_h <- aphMelt(dt0_h)
  }
  
  bycols <- c("hr", foi, "processName")
  
  dtl <- copy(dt0_h[processName %in% yoi])
  if ("dateTime" %in% names(dtl)) dtl[, dateTime := NULL]
  
  if (!"hr" %in% names(dtl)){
    dtl[, doy := yday(local_time)]
    dtl[, hr := hour(local_time)]
  }
  
  if ("wk" %in% c(foi, facet_w) & !"wk" %in% names(dtl)){
    dtl <- addTimeRes(dtl, "wk")
    bycols <- union(bycols, "wk")
  }
  if ("mon" %in% c(foi, facet_w) & !"mon" %in% names(dtl)){
    dtl <- addTimeRes(dtl, "mon")
    bycols <- union(bycols, "mon")
  }
  
  if (facet_w == "wk"){
    dtl[, wk := isoweek(local_time)]
    bycols <- c(bycols, "wk")
  }
  .dtl <<- copy(dtl)
  
  dtl.wagg <- dtl[
    , .(value = hmean(value)
        , sd = hsd(value)
        , rmse = hsd(value)/sqrt(.N)
        , hmin = hmin(value)
        , hmax = hmax(value) )
    , by = bycols]
  
  if (length(ribbon_sds)){
    if (ribbon_sds > 0){
      log_info("dayProfile|ribbon_sds taken as SD multiplier..")
      dtl.wagg[, hmin := value - ribbon_sds * sd]
      dtl.wagg[, hmax := value + ribbon_sds * sd]
    } else {
      log_info("dayProfile|ribbon_sds taken as rmse multiplier..")
      dtl.wagg[, hmin := value - abs(ribbon_sds) * rmse]
      dtl.wagg[, hmax := value + abs(ribbon_sds) * rmse]
    }
  }
  aphKey(dtl.wagg)
  pggsInput0 <- list(xoi = "hr"
                     , foi = foi
                     , facet_w = facet_w
                     , geom = geom
                     , xtics = 6
                     , xlab = xlab
                     , ribbonColor = foi #"cropseason_id"
  )
  pggsInput <- mergeParameters(pggsInput0, input
                               # , na = c("keep1", "keep2")
                               )
  print(pggsInput$xlab)
  .dtl.wagg <<- dtl.wagg
  p <- pggs(dtl.wagg
            , input = pggsInput
            , ...
  )
  
  if (addTraces){  
    p <- pggs(dtl
              , input = pggsInput
              # , facet_w = "mon", foi = "mon"
              , geom = "line"
              , group = c(aphFactors(dtl), "doy")
              , p = p
              , legend = "none"
              , lwd = .1
              # , lineColor = "red"
              , lineAlpha = .4)
  }
  
  structure(p, data = dtl.wagg)
}
