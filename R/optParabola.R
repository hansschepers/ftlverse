#' optParabola
#' 
#' @export
optParabola <- function(Temp, tempRef, tempTol){
  pmax(0, 1 - ((Temp - tempRef)/tempTol)^2)
}
