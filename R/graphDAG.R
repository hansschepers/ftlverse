#' graphDAG
#' @examples \dontrun{
#'   mmd <- graphDAG(dag)
#'   DiagrammeR::mermaid(paste(mmd, collapse = "\n"))
#' }
#' @export
graphDAG <- function(dag
                     , upars = list(daysPerWeek = 7, J2W = 8.64, gramPerKg = 1000, daysPerYear = 365) #universalConstants()
                     , hidePars = TRUE
                     , stopAt = "plantLoad"
                     , wrapSep = "<BR>"
                     , orientation = c("TB", "LR")[1]){
  mmd <- paste("graph", orientation)
  j <- length(mmd)
  i <- 1
  targetNameDone <- character(0)
  for (i in seq_along(dag)){
    targetName <- names(dag[[i]]$target)
    targetNameDone <- c(targetNameDone, targetName)
    av <- all.vars(parse(text = dag[[i]]$form))
    dependsOn <- setdiff(av, setdiff(names(upars), targetNameDone))
    if (hidePars) dependsOn <- setdiff(dependsOn, names(dag[[i]]$pars))
    for(dd in dependsOn){
      j <- j + 1
      if (wrapSep == "<BR>"){
        dd <- paste0(dd, "[", wrapCamel(dd, sep = "<BR>"), "]")
      }
      mmd[j] <- paste0(dd, " --> ", targetName)
    }
    if (names(dag[[i]]$target) == stopAt) break
  }
  mmd
}
