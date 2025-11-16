#' addSeasonalPhases
#' 
#' changes by reference!
#' @examples \dontrun{
#'   addSeasonalPhases(data.table(WAP = 1:50))[]
#'   addSeasonalPhases(data.table(year = 2023, wk = 1:50), plantingDate = as.Date("2023-01-17"))[]
#' }
#' 
#' @export
addSeasonalPhases <- function(dt
                              , phase.s = c(rep(1, 12)
                                            , rep(2, 13)
                                            , rep(3, 10)
                                            , rep(4, 50))
                              , plantingDate){
  dt <- copy(as.data.table(dt))
  
  if ("WAP" %in% names(dt)){
    dt[WAP >= 0, phase := phase.s[WAP+1]]
    dt[WAP < 0, phase := NA]
    return(dt[])
  }
  
  if (!"WAP" %in% names(dt)){
    if (missing(plantingDate)){
      log_error("addSeasonalPhases | please provide plantingDate")
      return(invisible(dt))
    }
    log_debug("column `WAP`is not found in dt - making based on dateTime, local_time or dDate?!")
    if ("local_time" %in% names(dt)){
      dt[, WAP := floor(as.numeric(difftime(local_time, plantingDate, units = "days"))/7)]
    }
    if ("dateTime" %in% names(dt)){
      dt[, WAP := floor(as.numeric(difftime(dateTime, plantingDate, units = "days"))/7)]
    }
    if ("dDate" %in% names(dt)){
      dt[, WAP := floor(as.numeric(difftime(dDate, plantingDate, units = "days"))/7)]
    }
    if (!"WAP" %in% names(dt)){
      if (all(c("year", "wk") %in% names(dt))){
        dt[, WAP := floor(as.numeric(difftime(ISOdate(year,1, 1) + lubridate::weeks(wk)
                                              , plantingDate, units = "days"))/7)]
      }
    }
  }
  if (!"WAP" %in% names(dt)){
    log_error("addSeasonalPhases | Cannot find column WAP, or dateTime, local_time or dDate")
    return(invisible(dt))
  }
  dt[WAP >= 0, phase := phase.s[WAP+1]]
  dt[WAP < 0, phase := NA]
  
  dt[]
}
