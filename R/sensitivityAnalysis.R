#' sensitivityAnalysis
#' @export
sensitivityAnalysis <- function(parScalings
                                , func
                                , times
                                , stateNames
                                , initExtension = ".init"
                                , fixedParms = list()
                                , doExp = FALSE
                                , doScale = TRUE
                                , odeMethod = "rk4"
                                , drivers = list()
                                # , correlKpi = "resid"
                                , Data = NULL){
  parmsNames <- parScalings$parameter
  # set up par mutations
  {
    unitLoads <- makeUnitLoadings(nms=c("base", parmsNames))
    df.scores <- unitLoads
    sensitivitySamplesDt <- getPcaScenarioDT(df.scores = df.scores
                                             , loads = unitLoads
                                             , freeSensParms = parmsNames
                                             , parScalings = parScalings
                                             , showPCs = FALSE
                                             , doExp = FALSE)  # always FALSE!!
    sensitivitySamplesDt$base <- 0
    sensitivitySamplesDt$scenId <- c("base", parmsNames)
    .sensitivitySamplesDt <<- copy(sensitivitySamplesDt)
    # dim(sensitivitySamplesDt)
    sensitivitySamplesDt
  }
  # run sensitivity
  if (exists(".simList", envir = .GlobalEnv)) {
    rm(.simList, envir = .GlobalEnv)
  }
  simList <- list()
  isim <- 1
  for (isim in seq(nrow(sensitivitySamplesDt)) ){
    runId <- sensitivitySamplesDt$scenId[isim]
    log_info(paste(isim, runId))
    if (runId %in% parmsNames){
      log_info("{runId} = {sensitivitySamplesDt[isim, ..runId]}")
    }
    editParms <- as.list(sensitivitySamplesDt[isim, ..parmsNames])
    # simList[[runId]] <- runFun(editParms = editParms)
    simList[[runId]] <- OdeModel(parms = editParms
                                 , func = func
                                 , times = times
                                 , stateNames = stateNames
                                 , initExtension = initExtension
                                 , fixedParms = fixedParms
                                 , odeMethod = odeMethod
                                 , doExp = doExp
                                 , drivers = as.list(drivers))
  }
  simList
}

# sensLong <- wrangleSimList(sensRuns = simList
#                            , baserunName = "base"
#                            , objectToExtract = 1
#                            , doi = "time"
# )
# sensLong[]



# 
# if(F) {
#   # dtw1 <- dcast(sensLong, time ~ sensPar) 
#   # } else {
#   #   # enrichDTsimple(dt)
#   #   # wrangle to loads
#   #   dt <- simList %>% 
#   #     lapply(as.data.table) %>% 
#   #     rbindlist(fill = TRUE, idcol = "sensPar") 
#   #   dt[, sensPar := factor(sensPar, levels = unique(sensPar), ordered = TRUE)]
#   #   dt
#   #   # dtw1 <- dcast(dt, time ~ parms, value.var = correlKpi) 
#   #   # dtw1
#   #   
#   #   dt <- melt(dtw1, id.vars = c("time", "base"), variable.name = "parms")
#   #   dt[, resid := value - base]
#   # }
#   
#   if (doScale){
#     sensLong[, resid := scale(resid), by = sensPar]
#   }
#   
#   .sensLong <<- copy(sensLong)
#   # sensLong <- copy(.sensLong)
#   dd <- copy(sensLong)
#   if (F){
#     sensLong[, variable := as.character(variable)]
#     unique(sensLong[, okData(resid), by = .(variable, sensPar)][V1 == 100]$variable)
#     
#     sensLongProblem <- sensLong[is.na(resid)
#                                 , .N
#                                 , by = .(variable, sensPar)]
#     
#     noSensVars <- character(0)
#     if (nrow(sensLongProblem)){
#       noSens <- dcast(sensLongProblem
#                       , sensPar ~ variable, value.var = "N")
#       noSens
#       noSensVars <- names(noSens)
#     }
#     # sensLong[, effectCombis := paste0(variable, sensPar)]
#     # noSens2 <- sensLong[, .(sd = hsd(resid)), by = effectCombis]
#     # noEffectCombis <- noSens2[sd == 0, effectCombis]
#     
#     noSens3 <- sensLong[, .(sd = hsd(resid)), by = sensPar]
#     noEffectPars <- noSens3[sd == 0, sensPar]
#     dd <- sensLong[!(variable %in% noSensVars
#                      | sensPar %in% noEffectPars)]
#     dd
#   }
#   #######################################################
#   # loads <- as.data.frame(loads)
#   detachedPars <- setdiff(parmsNames, names(sensMat))
#   # setdiff(names(sensMat), parmsNames)
#   colnames(loads) <- paste0("PC", seq(nrow(loads)))
#   row.names(loads) <- names(sensMat) #parmsNames
#   
#   np <- length(parmsNames)
#   loadsComplete <- matrix(data = 0, nrow = np, ncol = np
#                           , dimnames = list(pars = parmsNames
#                                             , pcs = paste0("PC", seq(np))))
#   loadsComplete[row.names(loads), colnames(loads)] <- loads
#   # detachedPars %in% parmsNames[ apply(loadsComplete, 1, function(x) sum(abs(x))) < 1e-4]
#   if (FALSE){
#     ww <- 0*loads[seq_along(detachedPars),]
#     row.names(ww) <- detachedPars
#     loads2 <- rbind(loads, ww)
#     
#     ww <- 0*loads2[,seq_along(detachedPars)]
#     colnames(ww) <- paste0("PC", seq(ncol(loads)+1, np))
#     loads3 <- cbind(loads2, ww)
#     loads4 <- loads3[row.names(loadsComplete),]
#     all.equal(as.numeric(loads4), as.numeric(loadsComplete))
#   }
#   loadsComplete <- loadsComplete[c(row.names(loads), detachedPars),]
#   if (FALSE){
#     all.equal(as.numeric(loads3), as.numeric(loadsComplete))
#   }
#   
#   return(list(loads = loads
#               , loadsComplete = loadsComplete
#               , corMat = corMat
#               , explainedVariance = explainedVariance
#               , sensMatFull = sensMatFull
#               , sensLong = sensLong
#               , detachedPars = detachedPars
#               # , simList = simList
#               , sensitivitySamplesDt = sensitivitySamplesDt))
# }

# qq <- hspca0(co = corMat)
# outpca <- attr(qq, "outpca")
