#' addTimeRes
#' @export
addTimeRes <- function(dfg
                       , toAdd = c("wk", "yr", "month")[1]
){
  if ("wk"%in% toAdd){
    log_debug("column `wk`is not found in dfg, but needed?! - making based on dateTime, local_time or dDate?!")
    if ("local_time" %in% names(dfg)){
      dfg$wk <- aphWeek(dfg$local_time)
    }
    if ("dateTime" %in% names(dfg)){
      dfg$wk <- aphWeek(dfg$dateTime)
    }
    if ("dDate" %in% names(dfg)){
      dfg$wk <- aphWeek(dfg$dDate)
    }
  }
  
  if ("yr"%in% toAdd){
    log_debug("column `yr`is not found in dfg, but needed?! - making based on dateTime, local_time or dDate?!")
    if ("local_time" %in% names(dfg)){
      dfg$yr <- year(dfg$local_time)
    }
    if ("dateTime" %in% names(dfg)){
      dfg$yr <- year(dfg$dateTime)
    }
    if ("dDate" %in% names(dfg)){
      dfg$yr <- year(dfg$dDate)
    }
  }
  
  if ("mon"%in% toAdd){
    log_debug("column `mon`is not found in dfg, but needed?! - making based on dateTime, local_time or dDate?!")
    if ("local_time" %in% names(dfg)){
      dfg$mon <- lubridate::month(dfg$local_time)
    }
    if ("dateTime" %in% names(dfg)){
      dfg$mon <- lubridate::month(dfg$dateTime)
    }
    if ("dDate" %in% names(dfg)){
      dfg$mon <- lubridate::month(dfg$dDate)
    }
  }
  
  if ("doy"%in% toAdd){
    log_debug("column `doy`is not found in dfg, but needed?! - making based on dateTime, local_time or dDate?!")
    if ("local_time" %in% names(dfg)){
      dfg$doy <- lubridate::yday(dfg$local_time)
    }
    if ("dateTime" %in% names(dfg)){
      dfg$doy <- lubridate::yday(dfg$dateTime)
    }
    if ("dDate" %in% names(dfg)){
      dfg$doy <- lubridate::yday(dfg$dDate)
    }
  }
  
  if ("hr"%in% toAdd){
    log_debug("column `hr`is not found in dfg, but needed?! - making based on dateTime, local_time or dDate?!")
    if ("local_time" %in% names(dfg)){
      dfg$hr <- lubridate::hour(dfg$local_time)
    }
    if ("dateTime" %in% names(dfg)){
      dfg$hr <- lubridate::hour(dfg$dateTime)
    }
    if ("dDate" %in% names(dfg)){
      dfg$hr <- lubridate::hour(dfg$dDate)
    }
  }
  
  dfg
}