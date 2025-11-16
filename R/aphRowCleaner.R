#' aphRowCleaner
#' 
#' @export
aphRowCleaner <- function(DT
                          , do = c("remove1rowfactorRows")
                          , foi){
  DT <- copy(DT)
  if ("remove1rowfactorRows" %in% do){
    foiRows <- DT[, .N, by = c(foi)]
    foisToRemove <- foiRows[N == 1, ..foi]
    DT <- DT[!get(foi) %in% foisToRemove]
  }
  return(DT)
}