#' getPcaScenarioDT
#' 
#' @param parScalings data.table
#' @examples \dontrun{
#'   df.scores <- makeScoresGrid(length.out= 3, ra=1, dims=2)
#'   df.scores <- makeScoresGrid(length.out=11, ra=2, dims=1)
#'   scenDT <- getPcaScenarioDT(df.scores = df.scores)
#'   scenDT
#'   
#'   df.scores <- makeScoresGrid(length.out=11, ra=2, dims=1)
#'   parScalings <- list(parameter = c("r", "K", "N0")
#'                         , pca.center = c(0.2, 100, 1)
#'                         , pca.sd = c(0.1, 100, 1))
#'   scenDTlogist <- getPcaScenarioDT(df.scores = df.scores
#'                                , freeSensParms = c("r", "K", "N0")
#'                                , parScalings = parScalings)
#'   scenDTlogist
#'   funInfo("getPcaScenarioDT")
#' }
#' @import data.table
#' @export
getPcaScenarioDT <- function( df.scores = makeScoresGrid(length.out = 5, dims=1)
                              , scenId = seq(nrow(df.scores))
                              , freeSensParms = "fw_max"
                              , loads = makeUnitLoadings(freeSensParms)
                              , parScalings# = getParamScalings2(freeSensParms = freeSensParms
                                          #                      , parms = list(fw_max = 200)
                                           #                     , relSD = .1)
                              , showPCs = TRUE
                              , doExp = FALSE
){
  
  noLoadingsAvailable <- setdiff(freeSensParms, row.names(loads))
  if (length(noLoadingsAvailable)) log_warn("no loading found for {noLoadingsAvailable}..")
  
  # print(df.scores)
  # print(loads)
  # print(freeSensParms)
  
  loads2 <- loads[intersect(row.names(loads), freeSensParms), ,drop = FALSE]
  usedLoads <- loads2 # as.matrix(loads2[, seq(ncol(df.scores))])
  # if(nrow(usedLoads) == 1) row.names(usedLoads) <- freeSensParms
  
  # print(loads2)
  # print(usedLoads)
  # message("------------------45")
  # print(tcrossprod(df.scores, usedLoads))
  
  parScalings <- as.data.table(parScalings)
  # add loadings per parameter
  scenDT <- cbind(data.frame(scenId)
                  , df.scores
                  , tcrossprod(df.scores, usedLoads)  ################## HERE <<<<<<<<<----------------
                  , stringsAsFactors = FALSE
  )
  setDT(scenDT)
  print(scenDT)
  
  # head(scenDT,13)
  # melt
  scen.long <- melt(scenDT
                    , id.vars = c("scenId", colnames(df.scores))
                    , variable.name = "parameter")
  setDT(scen.long)
  # hstr(scen.long)
  scen.long[, parameter := as.character(parameter)]
  
  # join 
  m1 <- parScalings$parameter
  m2 <- unique(scen.long$parameter)
  common <- intersect(m1, m2)
  # intersect(m1, m2)
  if (length(common) == 0){
    log_error("parScaling and model (free) parameters don't have enough overlap: {paste(length(m1), length(m2), length(common))}")
    # scen.long <<- scen.long
    # parScalings <<- parScalings
  }
  
  scen.long2 <- parScalings[scen.long, on = "parameter"]
  if (doExp){
    scen.long2[, valuePar := exp(pca.center) + exp(pca.sd) * value]
  } else {
    scen.long2[, valuePar := pca.center + pca.sd * value]
  }
  scen.long2[, parameter := factor(parameter, levels=unique(parameter), ordered=TRUE)]
  nmsPCs <- NULL
  if (showPCs) nmsPCs <- colnames(df.scores)
  formu <- paste(paste(c("scenId", nmsPCs), collapse=" + "), "parameter", sep=" ~ ")
  scenDT <- dcast(scen.long2, formu, value.var = "valuePar")
  scenDT
}

