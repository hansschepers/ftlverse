#' dtAllFactorsToCharacter
#' 
#' @export
dtAllFactorsToCharacter <- function(dt){
  fkt_idx = which(sapply(dt, is.factor))
  dt[ , (fkt_idx) := lapply(.SD, as.character)
      , .SDcols = fkt_idx]
  return(dt)
}