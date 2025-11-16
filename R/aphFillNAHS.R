#' aphFillNAHS
#'
#' @param DT The data.table to be aligned
#' @param group grouping variables
#' @param nsec The time interval in seconds (defaults to 300) to be used. For
#'   week aligment it is possible to use the value 'week'. All other
#'   non-numerics will result in en error
#' @param maxInterp What is the maximum time range that missing data should be
#'   imputed with linear interpolation (default 3600 seconds)
#' @param fillNA Should NA's be used if they cannot be interpolated (defaults to
#'   FALSE)
#' @details Missing data points can be filled with \code{NA} or interpolated
#'   where the maximum gap between two subsequent measurements of a specific
#'   sensor is less than specified by \code{maxInterp} (in seconds). If
#'   \code{maxInterp < nsec} no missing data points are filled. If
#'   \code{fillNa=FALSE} and \code{maxInterp >= nsec} missing data are
#'   interpolated with a maximum sequence of missing data points
#'   \code{maxInterp/nsec}.
#'
#' @importFrom stats start end complete.cases
#' @importFrom zoo na.approx
#'
#' @return Extended data.table with equidistant times.
#'
#' @export
aphFillNAHS <- function(DT
                        , group = aphFactors(DT)
                        , maxInterp = 3600
                        , nsec = 300
                        , doi = aphTimes(DT)[1]
                        , fillNA = TRUE
                        , keepNAafterFill = TRUE) {
  timeRanges <- DT[, .(start = hmin(get(doi))
                       , end = hmax(get(doi)))
                   , by = group]
  timeRanges <- timeRanges[, setNames(list(doi = seq(start, end, by = nsec)), doi)
                           , by = group]
  
  DT <- DT[timeRanges, on = c(doi, group)]
  fillNames <- setdiff(names(DT), c(doi, group, "value"))
  if (length(fillNames) > 0) {
    fillCols <- DT[complete.cases(base::get(fillNames))
                   , unique(.SD)
                   , by = group
                   , .SDcols = fillNames
                   ]
    DT[, c(fillNames) := fillCols[.GRP, .SD, .SDcols = fillNames], by = group]
  }
  if (fillNA) {
    DT[, value := as.numeric(zoo::na.approx(value,
                                          na.rm = FALSE, # keep remaining trailing NA's (begin or end)
                                          maxgap = as.integer(maxInterp / nsec)
  )), by = group]
  }
  # remove non-replaced NA's
  if (!keepNAafterFill) DT <- DT[!is.na(value)]
  return(DT)
}
