#' addCuts
#' @examples
#' \dontrun{
#'   p <- dayProfile(aphMelt(dfg), facet_w = "mon", yoi = "GHCO2C", ribbon_sds = 1, addTraces = TRUE)
#'   p
#'   ddm <- addCuts(dfg, tocut = list(VELSNI = 1))
#'   ddm <- addCuts(dfg, tocut = list(VELSNI = c(1, 5, 30)))
#'   ddm <- addCuts(dfg, tocut = list(VELSNI = c(1, 3, 30)))
#'   pggs2(ddm, yois = "GHCO2C", facet_w = "mon")
#'   pggs2(ddm, yois = "GHTEMP", facet_w = "mon")
#'   pggs(ddm, xoi = "VELSNI", yoi = "GHCO2C", facet_w = "state_VELSNI"
#'                  , ci.alpha = .1, geom = "point", foi = "mon", free_x = TRUE)
#'   ddm2 <- addCuts(dfg, tocut = list(GHCO2C = c(450, 600)))
#'   ddm2 <- addCuts(dfg, tocut = list(GHTEMP = c(19, 21)))
#'   str(ddm2)
#'   pggs2(ddm2, yois = "growthRate", facet_w = "mon", palette.oi = c("red", "orange", "green"))
#' }
#' 
#' @export
addCuts <- function(dfg
                    , tocut = list(VELSNI = .1)
                    , ordered_result = TRUE){
  ddm <- copy(dfg)
  nm <- names(tocut)[1]
  for (nm in names(tocut)){
    
    if (length(tocut[[nm]]) == 1){
      ddm[, (paste0("state_", nm)) := cut(
        get(nm)
        , breaks = c(-Inf, tocut[[nm]], Inf)
        , labels = paste(c("low", "high"), nm, sep = "_") 
        , ordered_result = ordered_result)]
    }
    
    if (length(tocut[[nm]]) == 2){
      ddm[, (paste0("state_", nm)) := cut(
        get(nm)
        , breaks = c(-Inf, tocut[[nm]], Inf)
        , labels = paste(c("low", "mid", "high"), nm, sep = "_") 
        , ordered_result = ordered_result)]
    }
    
    if (length(tocut[[nm]]) > 2){
      ddm[, (paste0("state_", nm)) := cut(
        get(nm)
        , breaks = c(-Inf, tocut[[nm]], Inf)
        , labels = paste(paste0("L:", seq(1+length(tocut[[nm]]))), nm, sep = "_")
        , ordered_result = ordered_result)]
    }
    
  }
  ddm[]
}
