#' aphCorrelations
#' @export
aphCorrelations <- function(dtl
                            , bycols = "cropseason_id"
                            , yois = aphVariableLevels(dtl)
                            , okDataThreshold = 85
                            , lag.max = 0
                            , kpi = "r"
                            , keep.tri = c("lower", "upper")[0]
){
  dtmpw <- copy(dtl)
  # auto-go wide for correlations
  fois <- union(aphFactors(dtmpw), bycols)
  message(fois)
  if ("processName" %in% names(dtmpw)) {
    dtmpw <- dtmpw[processName %in% yois]
    dtmpw <- hdcast(dtmpw, fois = fois)
  } else {
    if (!length(aphTimes(dtmpw))){
      dtmpw[, doy := .I]
    }
    dtmpw <- aphMelt(dtmpw, fois = union(aphTimes(dtmpw), fois))
    dtmpw <- dtmpw[processName %in% yois]
    dtmpw <- hdcast(dtmpw)
  }
  
  aphKey(dtmpw)
  .dtmpw <<- copy(dtmpw)
  
  actualyois <- setdiff(aphVariableLevels(dtmpw), aphTimes(dtmpw))
  
  dataQualityNAs <- sapply(dtmpw[, ..actualyois], okData)
  tooManyNAs <- actualyois[dataQualityNAs < okDataThreshold]
  log_warn("taking out variables: {tooManyNAs}")
  
  actualyois <- setdiff(actualyois, tooManyNAs)
  
  dmine <- dtmpw[complete.cases(dtmpw[, ..actualyois])]

  dmine_hr2 <- dmine[, (as.data.table(hr2dt(.SD
                                             , lag.max = lag.max
                                             , kpi = kpi
                                             , keep.tri = keep.tri
                                            ), keep.rownames = T))
                      , by = c(bycols)
                      , .SDcols = actualyois]
  
  # dmine_cors <- dmine[, (as.data.table(as.data.frame(
  #   cor(.SD, use = "pairwise.complete.obs"
  #       , method = c("pearson", "kendall", "spearman")[1])
  #   ), keep.rownames = T))
  #   , by = c(bycols)
  #   , .SDcols = actualyois]
  # .dmine_cors <<- copy(dmine_cors)
  # 
  # dmine_hr2 <- dmine[, (as.data.table(as.data.frame(
  #   hr2dt(.SD)), keep.rownames = T))
  #   , by = c(bycols), .SDcols = actualyois]
  .dmine_hr2 <<- copy(dmine_hr2)
  # dmine_corsSDX <- dmine[, lapply(.SD, hsd)
  #                        , by = c(bycols)
  #                        , .SDcols = actualyois]
  
  # .dmine_corsSDX <<- copy(dmine_corsSDX)
  # dmine_cors
  dmine_hr2
}