#' predictModel
#' can be called predict.aphMODEL, to function as S3 method for class 'aphMODEL'
#' @export
predictModel <- function(MODEL
                         , newdata
                         , modelId
                         , action = c("addpred", "future", "fillna", "replace")[1:2]
                         , addModelVersion = FALSE
                         , cleanupAddedPredictors = TRUE
                         , verbosity = 1
){
  if (missing(newdata) || is.null(newdata)){
    newdata <- copy(attr(MODEL, "origData"))
  }
  dtw <- copy(newdata)
  .dtw.b <<- copy(dtw)
  if (missing(modelId)){
    modelId <- attr(MODEL, "modelId")
  }
  lhs <- attr(MODEL, "lhs")
  rhs <- attr(MODEL, "rhs")
  toRemove <- character(0)
  if (("weekno" %in% rhs) & !"weekno" %in% names(dtw)){
    dtw[, weekno := lubridate::isoweek(dateTime)]
    toRemove <- c(toRemove, "weekno")
  }
  
  dtPredictors <- dtw[, ..rhs]
  iico <- complete.cases(dtPredictors)
  # .iico <<- iico
  dtw[iico, tmp2 := predict(MODEL
                            , na.action = na.pass
                            , newdata = dtPredictors[iico] ) ]
  # dtw$tmp2
  
  if ("replace" %in% tolower(action)){
    if (lhs %in% names(dtw)){
      dtw[, (lhs) := tmp2]
    } else {
      log_error("lhs not in dt {lhs}")
    }
  } 
  if ("fillna" %in% tolower(action)){
    if (lhs %in% names(dtw)){
      dtw[is.na(get(lhs)), (lhs) := tmp2]
    } else {
      log_error("lhs not in dt {lhs}")
    }
  }
  
  if ("future" %in% tolower(action)){
    if (lhs %in% names(dtw)){
      if ("timeHorizon" %in% names(dtw)){
        dtw[timeHorizon > 0, (lhs) := tmp2]
      } else {
        log_error("'timeHorizon' not in dt")
      }
    } else {
      log_error("lhs not in dt {lhs}")
    }
  }
  
  if ("addpred" %in% tolower(action)){
    predVar <- paste0(lhs, ".pred")
    if (predVar %in% names(dtw)){
      log_warn("removing pre-existing prediction-column {predVar}")
    }
    dtw[, (predVar) := tmp2]
  }
  
  if (addModelVersion){
    if (lhs %in% names(dtw)){
      dtw[, tmp1 := get(lhs)]
      setnames(dtw, "tmp1", modelId)
    }
    predVar <- paste0(modelId, ".pred")
    if (predVar %in% names(dtw)){
      log_warn("removing pre-existing prediction-column {predVar}")
    }
    setnames(dtw, "tmp2", predVar)
  } else {
    dtw[, tmp2 := NULL]
  }
  if (verbosity > 0){
    relevantColumns <- grep(paste0("^", lhs), names(dtw), value = TRUE)
    print(relevantColumns)
    print(dtw[, ..relevantColumns])
  }
  if (cleanupAddedPredictors){
    if (length(toRemove) > 0) {
      dtw[, (toRemove) := NULL]
    }
  }
  
  return(dtw[])
}
