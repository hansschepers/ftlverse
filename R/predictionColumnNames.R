#' predictionColumnNames
#' 
#' @export
predictionColumnNames <- function(dt
                                  , addTruthColumn = TRUE
                                  , addActualColumn = TRUE
                                  , predExtension = "\\.pred$"
                                  , truthExtension = c(".actual", "")[1]
                                  ){
  predNms <- names(dt)[grepl(predExtension, names(dt))]
  baseNms <- sub(predExtension, "", predNms)
  actualNms <- sub(predExtension, truthExtension, predNms)
  ok <- (baseNms %in% names(dt)) | (actualNms %in% names(dt))
  if (any(!ok)){
    log_warn("missing baseNms in dt: {baseNms[!ok]}")
    baseNms <- baseNms[ok]
    predNms <- predNms[ok]
  }
  if (addTruthColumn){
    predNms <- c(predNms, baseNms)
  } 
  if (addActualColumn){
    predNms <- c(predNms, actualNms)
  } 
  sort(predNms)
}

