#' addDAPOLD
#' @description adds, `transplant_date` and its derived time variables
#' TAP = Time After Transplant (numeric, in days)
#' DAP = Days After Transplant (integer)
#' WAP = Weeks After Transplant (integer)
#' 
#' @param dt a `data.table` at least containing columns `cycle_syn` and, depending on colsToAdd, also `get(doi)`
#' @param cycleMeta, optional, a `datat.table` with results of 
#' @param doi [dateTime]
#' @param colsToAdd [colsToAdd = c("start_date", "end_date", "sowing_date", "transplant_date", "TAP", "DAP", "WAP")[c(4, 6, 7)]]
#' 
# @export
addDAPOLD <- function(dt = NULL
                   , cycleMeta = getDBXGS()$cropseason_wide
                   , doi = intersect(c("local_time", "dateTime", "dDate"), names(dt))[1]
                   , colsToAdd = c("start_date", "end_date"
                                   , "sowing_date", "transplant_date"
                                   , "TAP", "DAP", "WAP")[c(4, 6, 7)]
                   , doToMonday = TRUE
){
  if (!"transplant_date" %in% names(dt)){
    setDT(cycleMeta)
    if (!"cycle_syn" %in% names(cycleMeta)){
      setnames(cycleMeta, "cycle_name", "cycle_syn")
    }
    
    colsToAdd <- union("cycle_syn", colsToAdd)
    keep <- setdiff(colsToAdd, c("TAP", "DAP", "WAP"))
    if (any(c("TAP", "DAP", "WAP") %in% colsToAdd)) {
      keep <- union("transplant_date", keep)
    }
    
    if (doToMonday){
      cycleMeta$transplant_date <- toMonday(cycleMeta$transplant_date)
    }
    # wday(cycleMeta$transplant_date)
    
    ddd <- cycleMeta[is.na(cycleMeta$transplant_date)]
    if (nrow(ddd)){
      log_warn("unknown transplant_dates")
      # print(ddd)
    }
    # cycleMeta[, cycleLength := as.numeric(difftime(end_date, start_date, units = "weeks"))]
    # pggs(cycleMeta[end_date > Sys.time() & cycleLength < 800]
    #      , xoi = "cycleLength", yoi = "cycle_syn"
    #      , geom = "point", legend = "none")
    
    # join
    .cycleMeta <<- cycleMeta
    if (is.null(doi) | isTRUE(is.na(doi))) {
      log_warn("no date column {doi} found, returning whole cycleMeta data.table!!")
      return(cycleMeta)
    }
    setDT(dt)
    dt <- cycleMeta[, ..keep][copy(dt), on = "cycle_syn"][]
  }
  if (any(c("TAP", "DAP", "WAP") %in% colsToAdd)) 
    dt[, TAP := as.numeric(difftime(get(doi)
                                    , transplant_date, units = "days"))]
  if (any(c("DAP", "WAP")        %in% colsToAdd)) dt[, DAP := as.integer(TAP)]
  if ("WAP"                      %in% colsToAdd)  dt[, WAP := as.integer(DAP/7)]
  
  dt[]
}
