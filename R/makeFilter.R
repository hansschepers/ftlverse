#' makeFilter
#' @examples \dontrun{
#'   makeFilter()
#'   makeFilter(minYield = .3)
#'   makeFilter(minBrix = 3)
#'   makeFilter(minAFW = 30)
#' }
#' @export
makeFilter <- function(minYieldQuantile = 0
                       , minimumYield = 0
                       , minBrix = 0
                       , maxAFW = 500
                       , filters = list(
                         minYieldQuantile = minYieldQuantile
                         , minimumYield = minimumYield
                         , minBrix = minBrix
                         , maxAFW = maxAFW
                       )
                       , ...){
  dots <- list(...)
  if (length(dots)){
    log_info("using / making filter element beyond the defaults {names(dots)}")
  }

  mergeParameters(filters, dots)
}
