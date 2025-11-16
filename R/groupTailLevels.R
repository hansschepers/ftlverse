#' groupTailLevels
#'
#' @export
groupTailLevels <- function(dfg
                            , foi
                            , maxLevels = 50){
  foi <- intersect(foi, names(dfg))
  if (!length(foi)) {
    return(dfg)
  }
  wasDF <- is.data.frame(dfg)
  dfg <- as.data.table(dfg)
  tabl <- dfg[, .(N = uniqueN(get(foi)))][order(N)]
  .tabl <<- copy(tabl)
  print(tabl)
  if (wasDF) setDF(dfg)
  dfg
}
