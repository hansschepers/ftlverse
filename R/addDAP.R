#' addDAP
#' 
#' @export
addDAP <- function(dt
                   , meta = NULL
                   , transplant_date = "minimumDate"
                   , bycols = c("cropseason_id", "variety")
                   , doi = c("local_time", "dateTime")
                   , addBase = 1){
  bycols <- intersect(bycols, names(dt))
  
  if ("DAP" %in% names(dt)) {
    log_debug("'DAP' already present")
    return(dt)
  }
  doi <- intersect(names(dt), doi)[1]
  stopifnot(length(doi) > 0)
  
  dt <- copy(dt)
  if ("Time" %in% names(dt) & !"DAP" %in% names(dt)) {
    log_debug("DAP := floor(Time/1)")
    dt[, DAP := floor(Time /1)]
    return(dt)
  }
  
  if (!is.null(meta)){
    stopifnot("transplant_date" %in% names(meta))
    dt[meta
       , on = bycols,
       DAP := floor(as.numeric(addBase + difftime(get(doi), transplant_date
                                                  , units = "days"))/1)]
    return(dt)
  }
  
  bycols <- intersect(bycols, names(dt))
  if (is.character(transplant_date)){
    dt[, DAP := floor(as.numeric(addBase + difftime(get(doi), hmin(get(doi))
                                                    , units = "days"))/1)
       , by = bycols]
  }
  if (is.Date(transplant_date)){
    dt[, DAP := floor(as.numeric(addBase + difftime(get(doi), transplant_date
                                                    , units = "days"))/1)]
  }
  dt[]
}
