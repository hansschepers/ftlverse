# res <- aphVary(scenDT = .scenDT
#                     , nscenDT = 3
#                     , enrichPars = enrichPars
#                     , FUN = weeklyEnrichWideOrig
#                     , funArgs = list(DT = .ddww, addRowsTopredict = 5, bycols = "cycle_syn")
#                     )

#' aphVary
# @importFrom shinyWidgets progressBar updateProgressBar
#' @importFrom data.table copy
#' @export
aphVary <- function(scenDT
                    , nscenDT = nrow(scenDT)
                    , enrichPars = list()
                    , FUN = runFun # weeklyEnrichWideOrig
                    , funArgs = list()
                    , targetArg = "lastEditParms"
                    , targetArgIsList = TRUE
                    , includeNames = c("auto", "first")[1]
                    , omitNames = character(0)
                    , session = NULL
                    , pbId = "simsProgressBar"
                    , pbModulo = 2
                    , verbosity = log_threshold()
){
  scenDT <- data.table::copy(scenDT)
  varyResult <- list()
  scen <- 1
  for (scen in seq(nscenDT)){
    {
      gc()
      # if (length(enrichPars)){
      scenario <- as.list(scenDT[scen])
      # make message / progressBar
      {
        if (any(c("auto", "all") %in% includeNames)){
          includeNames <- names(scenario)
        }
        if (any(c("first") %in% includeNames)){
          includeNames <- names(scenario)[1]
        }
        # str(includeNames)
        modelHP <- scenario[includeNames]
        modelHP[omitNames] <- NULL
        toChar <- sapply(modelHP, inherits, "Date")
        modelHP[toChar] <- sapply(modelHP[toChar], as.character)
        # sapply(modelHP, class)
        hyperPars <- paste(paste(names(modelHP), unlist(modelHP), sep = "="), collapse = "  ")
        pbMessage <- paste(scen, "/", nscenDT, ":", hyperPars)
        # if (verbosity > 500){
        print(pbMessage)
        # }
      }
      # print(scenario)
      # listAddedToTarget <- mergeParameters(enrichPars, scenario)
      # listAddedToTarget <- mergeParameters(funArgs[[targetArg]], listAddedToTarget)
      # # str(listAddedToTarget)
      # funArgs[[targetArg]] <- listAddedToTarget
      if (targetArgIsList){
        funArgs[[targetArg]] <- mergeParameters(funArgs[[targetArg]], scenario)
      } else {
        funArgs[[targetArg]] <- unlist(scenario[[targetArg]])
      }
      # }
      # message("scen:", scen)
      # print(funArgs)
      if ("iseed" %in% names(scenario)){
        message("iseed handling")
        funArgs$iseed <- NULL
      }
      # if ("iseed" %in% names(scenario)){
      #   set.seed(scenario$iseed)
      # }

      .funArgs <<- funArgs
      cn <- compareNames(names(funArgs[[targetArg]]), names(formals(FUN)))
      for (cArg in cn$common){
        log_info("common elements: {cArg}")
        funArgs[cArg] <- funArgs[[targetArg]][[cArg]]
        if (cArg != "scenId") funArgs[[targetArg]][cArg] <- NULL
      }
      .funArgsRUN <<- funArgs

      # if (length(cn$unique1)){
      #   log_debug("possible crash if FUN does not have '...'as argument: {cn$unique1}")
      # }
      res <- do.call(FUN, funArgs)
      # simsPlot(res)

      if (inherits(res, "data.table")){
        res <- data.table::copy(res)
        res <- cbind(res, scenario)
      }

      # scenId <- hyperPars
      scenId <- scen
      varyResult[[scenId]] <- res
    }


    if (!is.null(session)){
      if (scen %% pbModulo == 0){
        updateProgressBar(session = session
                          , id = pbId
                          , value = scen
                          , total = nscenDT
                          , title = pbMessage)
      }
    }
  }
  return(varyResult)
}
