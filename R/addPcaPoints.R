#' addPcaPoints
#' @param GlobalPca output from \link{aphPca}
#' @export
addPcaPoints <- function(dt, GlobalPca
                         , yois
                         , fois
                         , maxPCs = 2
                         , PCsuffix = NULL
                         , include = c("dt", "yois", "fois")[1]
                         ){
  if ("fois" %in% include){
    if (missing(fois)){
      fois <- aphFactors(dt)
      log_debug("fois: {paste(fois, collapse = ',')}")
    }
    insertFois <- dt[, ..fois]
  } else {
    insertFois <- NULL
  }
  
  
  if (missing(yois)){
    yois <- aphVariableLevels(dt)
    log_debug("yois: {paste(yois, collapse = ',')}")
  }
  nn <- length(yois)
  nnPCs <- min(maxPCs, nn)
  dd.sc <- scale(as.matrix(dt[, ..yois])
                 , center = GlobalPca$center
                 , scale = GlobalPca$scale)
  scores <- as.data.table((dd.sc %*% GlobalPca$rotation)[, seq(nnPCs)])
  
  PCnames <- colnames(GlobalPca$rotation)[seq(nnPCs)]
  PCnamesOut <- paste0(PCnames, PCsuffix)
  setnames(scores, PCnames, PCnamesOut)
  
  insertYois <- NULL
  if ("yois" %in% include){
    insertYois <- dt[, ..yois]
  } else {
    insertYois
  }
  
  insertDT <- NULL
  if ("dt" %in% include){
    insertDT <- copy(dt)
  } else {
    insertDT
  }
  
  cbind(insertDT, insertFois, insertYois, scores)
}
