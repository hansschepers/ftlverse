#' waterVariablesToDaily
#' 
#' changes by reference!
#' from L/m2/hr to L/m2/day
#' 
#' @export
waterVariablesToDaily <- function(
    dt
    , yois = c("transpiration"
               , "cc_per_J_Transpired"
               , "Uptake", "waterUptake"
               , "Irrigation", "waterSupply"
               , "DrainLG", "drain")){
  if (!all(c("processName", "value") %in% names(dt))) return(invisible(dt))
  dt[processName %in% yois, value := value * 24][]
}
