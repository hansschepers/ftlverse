#' findDuplicates
#' 
#' finds exported functions only
#' @author hans.schepers
#' @examples \dontrun{
#'   findDuplicates(package1 = "data.table", package2 = "data.frame")
#' }
#' @export
findDuplicates <- function(package1 = "data.table"
                           , package2 = "data.frame"
                           ){
  log_info("{package1} x {package2}:")
  if (package1 == package2) return(character(0))
  funs1 <- createFunList(package1)[status == "export"]$fun
  funs2 <- createFunList(package2)[status == "export"]$fun
  duplicates <- intersect(funs1, funs2)
  return(duplicates)
}
