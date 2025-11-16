#' partiallyAlignToTruth
#' @examples \dontrun{
#'   victim = c(3, 3, 3, 4, 5, 6)
#'   truth = c(5, 5, 6, 5, 4, 4)
#'   partiallyAlignToTruth(victim, truth, 80)
#' }
#' @export
partiallyAlignToTruth <- function(victim, truth, alignPercentage){
  victim + (truth - victim) * alignPercentage/100
}
