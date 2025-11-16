#' addWAP
#' 
#' @export
addWAP <- function(dt
                   , meta = NULL
                   , transplant_date = "minimumDate"
                   , bycols = c("cropseason_id", "variety")
                   , doi = c("local_time", "dateTime")
                   , addBase = 1){
  bycols <- intersect(bycols, names(dt))
  
  if ("WAP" %in% names(dt)) {
    log_debug("'WAP' already present")
    return(dt)
  }
  doi <- intersect(names(dt), doi)[1]
  if(!length(doi) > 0){
    log_fatal("addWAP| dt has not time column ******************")
    print(dt)
    stop()
    return(dt)
  }
  
  dt <- copy(dt)
  if ("Time" %in% names(dt) & !"WAP" %in% names(dt)) {
    log_debug("WAP := floor(Time/7)")
    dt[, WAP := floor(Time /7)]
    return(dt)
  }
  
  if (!is.null(meta)){
    stopifnot("transplant_date" %in% names(meta))
    dt[meta
       , on = bycols,
       WAP := floor(as.numeric(addBase + difftime(get(doi), transplant_date
                                                  , units = "days"))/7)]
    return(dt)
  }
  
  bycols <- intersect(bycols, names(dt))
  if (is.character(transplant_date)){
    dt[, WAP := floor(as.numeric(addBase + difftime(get(doi), hmin(get(doi))
                                                    , units = "days"))/7)
       , by = bycols]
  }
  if (is.Date(transplant_date)){
    dt[, WAP := floor(as.numeric(addBase + difftime(get(doi), transplant_date
                                                    , units = "days"))/7)]
  }
  dt[]
}
