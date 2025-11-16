#' aphAggregate
#'
#' @export
aphAggregate <- function(dd
                         , expr = c("meanValue"
                                    , "medianValue"
                                    , "lbub"
                                    , "summary"
                                    , "hsummary(value)"
                                    , "none")[1]
                         , value.name = "value"
                         , accross = character(0)
                         , by = c("all")
                         , unit = c("none", "days", "weeks")[1]
                         , dateCol = c("local_time", "dateTime", "dDate", "Time") # autochosen
                         , week_start = 1
){
  if (! value.name %in% names(dd)) {
    log_trace("aphAggregate: no {value.name}-column, not aggregating..")
    return(dd)
  }
  if (as.character(expr)[1] == "none") return(dd)
  wasDT <- is.data.table(dd)
  dd <- copy(as.data.table(dd))
  if ("processName" %in% names(dd)){
    dd[, processName := as.character(processName)]
  }
  aphKey(dd)

  dateCol <- intersect(dateCol, names(dd))[1]
  log_debug("aphAggregate | dateCol {dateCol} {dim(dd)}")

  # by
  if ("all" %in% by){
    by <- c(aphFactors(dd), dateCol)
    by <- setdiff(by, NA)
    log_debug("by: {paste(by, collapse = ',')}")
  }

  if ("wk" %in% c(by)){
    if (!"wk" %in% names(dd)){
      log_warn("column `wk`is not found in dd, but needed?! - making based on dateTime or dDate?!")
      if ("dateTime" %in% names(dd)){
        dd$wk <- lubridate::isoweek(dd$dateTime)
      }
      if ("dDate" %in% names(dd)){
        dd$wk <- lubridate::isoweek(dd$dDate)
      }
    }
  }

  missingInDT <- setdiff(by, names(dd))
  if (length(missingInDT)) {
    message("missing in DT: ", paste(missingInDT, collapse = ", "))
    log_info("by {by}")
    str(accross)
    log_info("accross {accross}")
    
  }
  
  missingInBy <- setdiff(aphFactors(dd), by)
  if (length(missingInBy)) {
    message("aggregating accross: ", paste(missingInBy, collapse = ", "))
  }
  by <- intersect(names(dd), by)
  by <- setdiff(by, accross)
  log_info("by: {paste(by, collapse = ", ")}")
  
  
  log_debug("time aggregating on dateCol: {dateCol}")
  stopifnot(length(dateCol) > 0)

  # i / dateTime
  if (any(c("hr", "hour", "hours") %in% unit)){
    dd[, c(dateCol) := lubridate::floor_date(base::get(dateCol), unit = "hours")]
  }

  if (any(c("day", "days") %in% unit)){
    dd[, c(dateCol) := hfloor_date(base::get(dateCol))]
  }

  if (any(c("year", "years") %in% unit)){
    dd[, c(dateCol) := floor_date(base::get(dateCol), unit = "years")]
  }

  if (any(c("month", "months") %in% unit)){
    dd[, c(dateCol) := floor_date(base::get(dateCol), unit = "months")]
  }

  if (any(c("wk", "week", "weeks") %in% unit)){
    dd[, c(dateCol) := floor_date(base::get(dateCol), unit = "weeks"
                                     , week_start = week_start)]
  }


  # j aggregation
  if (inherits(expr, "character")){
    if (expr == "meanValue"){
      expr <- glue("list({value.name} = hmean({value.name}))")
    }
    if (expr == "medianValue"){
      expr <- glue("list({value.name} = hmedian({value.name}))")
    }
    if (expr[1] == "lbub"){
      expr <- glue("list(upperBound = hmax({value.name}), lowerBound = hmin({value.name}))")
    }
    if (expr[1] == "summary"){
      expr <- glue("hsummary({value.name})")
    }
    if (expr[1] == "ribbon"){
      expr <- glue("hsummary({value.name}, hfuns = c('hmin', 'hmax'))")
    }

    dda <- dd[, eval(parse(text = expr)), by = c(by)]
  } else {
    dda <- dd[, eval(expr), by = c(by)]
  }
  aphKey(dda)
  .dda <<- copy(dda)
  if (!wasDT) setDF(dda)
  return(dda[])
}


# if ("weeks" %in% unit){
#   expr <- expression(list(value = hmean(value), dDate = hmin(dDate)))
# }
# str(expr)

