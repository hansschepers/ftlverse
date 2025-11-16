#' MAE
#' @examples \dontrun{
#'   MAE(truth = c(1,-2,3), estimate = c(2,-3,5))
#'   MAE(c(1, 1, 2))
#' }
#' 
#' @export
MAE <- function(truth
                , estimate
                , x = truth - estimate
                , na.rm = TRUE){
  if (missing(estimate)){
    x <- truth
  }
  hmean(abs(x), na.rm = na.rm)
}

#' MAPE
#' @examples \dontrun{
#'   hMAPE(truth = c(1,-2,3), estimate = c(2,-3,4))
#'   hMAE(truth = c(1,-2,3), estimate = c(2,-3,4))
#'   residREL(truth = c(1,-2,3), estimate = c(2,-3,4))
#'   residRELABS(truth = c(1,-2,3), estimate = c(2,-3,4))
#'   hMAPE(truth = 3, estimate = 4)
#'   hMAPE(truth = 4, estimate = 3)
#'   hMAPE(truth = 3, estimate = NA)
#'   hMAPE(truth = NA, estimate = NA)
#'   hMAPE(truth = -3, estimate = -4)
#'   hMAPE(truth = -4, estimate = -3)
#'   hMAPE(truth = -4, estimate = 3)
#'   hMAPE(truth =  0, estimate = 3)
#'   hMAPE(truth =  -3, estimate = 0)
#'   hMAPE(truth = -3, estimate = -2, offset = 10)
#'   hMAPE(truth = -3, estimate = -2, offset = 1)
#'   hMAPE(truth = -3, estimate = -2, offset = 0)
#' }
#' @export
hMAPE <- function(truth
                  , estimate
                  , x = estimate - truth
                  , na.rm = TRUE, mima = 1, offset = 1
                  , BIAS = FALSE
                  ){
  if (missing(truth)){
    cond <- TRUE
    truth <- 0
  } else {
    cond <- hmin(truth) > 1e-4
    if (is.na(cond)) cond <- FALSE
    if (cond){
      offset <- 0
    } else {
      log_trace("adding offset to Truth before scaling hMAPE")
    }
  }
  # scal <- (offset + abs(truth))
  scal_t <- hmax(c(hrange(truth), hmax(abs(truth))))
  scal_e <- hmax(c(hrange(estimate), hmax(abs(estimate))))
  scal <- hmax(c(scal_t, scal_e))
  if (!BIAS) x <- abs(x)
  hmean(pmin(mima, pmax(-mima, x / scal ))
        , na.rm = na.rm)
}


#' hMAPE2
#' 
#' @export
hMAPE2 <- function(truth
                   , estimate
                   , x = truth - estimate
                   , na.rm = TRUE, mima = 1){
  # scal <- (offset + abs(truth))
  scal_t <- hmax(c(hrange(truth), hmax(abs(truth))))
  scal_e <- hmax(c(hrange(estimate), hmax(abs(estimate))))
  scal <- hmax(c(scal_t, scal_e))
  hmean(pmin(mima, pmax(-mima, abs(x) / scal ))
        , na.rm = na.rm)
}


#' hMAE
#' 
#' scaled BIAS  can be written as hMAPE with abs = FALSE
#'  
#' @export
hMAE <- function(...){
  hMAPE(..., BIAS = TRUE)
}
#' hBIAS
#' @export
hBIAS <- hMAE



#' hMASE
#' @export
hMASE <- function(truth
                  , estimate
                  , x = truth - estimate
                  , na.rm = TRUE, mima = 1, offset = 1){
  if (missing(truth)){
    cond <- TRUE
    truth <- 0
  } else {
    cond <- hmin(truth) > 1e-4
    if (is.na(cond)) cond <- FALSE
    if (cond){
      offset <- 0
    } else {
      log_trace("adding offset to Truth before scaling hMASE")
    }
  }
  hmean(pmin(mima, pmax(-mima, abs(x) / (offset + hmean(abs(truth))) ))
        , na.rm = na.rm)
}



#' RESID
#' 
#' @export
RESID <- function(truth, estimate, na.rm = TRUE){
  stopifnot(length(truth) == 1)
  hmean(truth - estimate, na.rm = na.rm)
}



#' residREL
#' 
#' @export
residREL <- function(truth, estimate, na.rm = TRUE){
  scal_t <- hmax(c(hrange(truth),    hmax(abs(truth))))
  scal_e <- hmax(c(hrange(estimate), hmax(abs(estimate))))
  scal <- hmax(c(scal_t, scal_e))
  (truth - estimate) / scal
}

#' residRELABS
#' 
#' @export
residRELABS <- function(truth, estimate, na.rm = TRUE){
  scal_t <- hmax(c(hrange(truth),    hmax(abs(truth))))
  scal_e <- hmax(c(hrange(estimate), hmax(abs(estimate))))
  scal <- hmax(c(scal_t, scal_e))
  abs(truth - estimate) / scal
}





#' RMSE
#' 
#' @export
RMSE <- function(x = truth - estimate, truth, estimate, na.rm = TRUE, ...){
  if (missing(estimate)){
    x <- truth
  }
  sqrt(mean((x)^2, na.rm = na.rm, ...))
}


#' summaryMetrics
#' 
#' @export
summaryMetrics <- function(DTpred
                           , metric.s = c(hMAPE = hMAPE
                                          , MAE = MAE
                                          # , RMSE=RMSE
                                          # , hlength=hlength
                           )
                           , byPerf = NULL
                           , variable.name = c("processName", "variable")[1]
                           , predExtension = "\\.pred$"
                           , truthExtension = c(".actual", "")[1]
){
  if ("switchDate" %in% byPerf){
    if (!"switchDate" %in% names(DTpred)){
      DTpred[, switchDate := 1]  #TODO or should this be a date?
    }
  }
  setDT(DTpred)
  predName.s <- predictionColumnNames(
    DTpred
    , addTruthColumn = FALSE
    , addActualColumn = FALSE
    , truthExtension = truthExtension #not necessary addActualColumn = FALSE
  )
  if (length(predName.s) == 0){
    message("no prediction columns found")
    return(NULL)
  }
  
  result <- list()
  # columnName <- predName.s[1]
  for (columnName in predName.s){
    truthColumnName <- sub(predExtension, truthExtension, columnName)
    if (!truthColumnName %in% names(DTpred)){
      # the Other truthExtension
      truthExtension <- setdiff(c(".actual", ""), truthExtension)
      truthColumnName <- sub(predExtension, truthExtension, columnName)
    }
    {
      diagn <- DTpred[, .(allNaTruth = all(is.na(get(truthColumnName)))
                          , allNaPred = all(is.na(get(columnName)))), by = byPerf]
      if (sumna(unlist(diagn)) > 0){
        if (log_threshold() > 500){
          log_warn("NA in either Truth' or Pred:")
          # print(diagn)
        }
      }
      # metricName <- names(metric.s)[1]
      for (metricName in names(metric.s)){
        # message(metricName)
        combi <- paste(truthColumnName, metricName, sep = "__X__")
        .combi <<- combi
        # metric.s[[metricName]](DTpred$afw.actual, DTpred$afw.pred)
        result[[combi]] <- DTpred[, .(.estimate = 
                                        metric.s[[metricName]](get(truthColumnName) #truth
                                                               , get(columnName) #estimate
                                        )
        )
        , by = byPerf]
      }
    } #else {
    #log_warn("truthColumnName {truthColumnName} not found in DTpred")
    # }
  }
  .result <<- result
  ww <- rbindlist(result, fill = TRUE, idcol = "combi")
  ww[, `:=`(c(variable.name, ".metric"), tstrsplit(combi, "__X__", fixed = TRUE))]
  ww[, combi := NULL]
  ww[]
}


# applyMetrics
# 
# @export
if(F){
  applyMetrics <- function(dList
                           , extraFuns = c("hMAPE", "MAE", "RMSE", "hlength")
                           , filterExpr = quote(MAE > 1e-5)
                           , ...){
    
    qq <- sapply(dList, hsummary, hfuns = extraFuns, simplify = F)
    qq <- rbindlist(qq, fill = T, idcol = "yoi")
    res[[ii]] <- qq
    # aphMelt(qq)
  }
  qq <- rbindlist(res, idcol = "sim")
  qq <- qq[eval(filterExpr)]
  return(qq)
}
