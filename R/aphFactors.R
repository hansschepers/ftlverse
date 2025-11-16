#' aphFactors
#'
#' a positive list for factors
#' @param DT a data.frame, list or data.table
#' @param caseSensitive logical asking to be case Sensitive
#' @examples \dontrun{
#'   (foisPos <- aphFactors(iris))
#' }
#' @export
aphFactors <- function(
  DT = NULL
  , caseSensitive = FALSE
  , addClasses = c("character", "factor")
  , fois = c("status", "phase", "cropPhase"
             , "account", "accountId", "account_id", "account_name"
             # , "department_id", "greenhouse_id"
             # , "plan_id", "variety_id", "segment_id", "crop_id", "location_id"
             # , "channel_id", "source_id"
             , "location_name"
             , "cycle_syn", "cycle_name", "cycle"
             , "differentiator", "name_department_letsgrow"
             , "field_syn", "field_name"
             , "plot_syn", "plot_name"
             , "cropseason_id", "cropseason_name"
             , "season"
             , "csId"
             , "variety", "variety_name", "rootstock", "rootstock_name"
             , "segment", "segment_name", "aphSegment", "segmentYield"
             , "sensor_type", "sensor_syn", "sensor_name"
             , "station", "station.name", "station.location"
             , "scenId", "lockedScenId"
             , "person"
             , "api", "cohortId", "age", "stage"  # from models (flowering, hanging, harvested e.d.)
             , "processName", "variable")
  , add_ids = TRUE
){
  if (is.null(DT)) {
    return(character(0))
  }
  nms <- names(DT)
  if (caseSensitive){
    fois <- intersect(fois, nms)
  } else {
    fois <- fois[tolower(fois) %in% tolower(nms)]
  }
  
  addFields <- character(0)
  if (add_ids) addFields <- grep("_id$", nms, value = TRUE)
  for (classOI in addClasses){
    addFields <- c(addFields, nms[sapply(DT, inherits, classOI)])
  }
  fois <- unique(c(fois, addFields))
  return(fois)
}


#' autoFactors (formerly afois)
#'
#' negative list for factors, includes dates, times etc.
#' @param DT a data.frame, list or data.table
#' @examples \dontrun{
#'   foisNeg <- autoFactors()
#' }
#' @export
autoFactors <- function(DT) {
  setdiff(names(DT), c("value", aphVariables(DT)))
}


#' autoFactorsNoDate
#'
#' @export
autoFactorsNoDate <- function(DT) {
  setdiff(names(DT), c("value", aphVariables(DT), aphTimes(DT)))
}
