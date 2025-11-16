#' aphScenarios
#' @export
aphScenarios <- function(
    scenDT
    , SIMS
    # , locationWeather = list(historic = data.table())
    # , drivers_weekData = data.table()
    , lastEditParms = list()
    , planOrigin = "aphScenarios_aphVary"
    , FUN = runFun
    , drivers = NULL
    , ...){
  {
    # names(SIMS)
    .scenDT000 <<- copy(scenDT)
    scenDT <- fillScenDT(scenDT, pars = SIMS$usedParms)

    # args2copy <- eval(formals(extractPlanSpecs)$itemsToExtract)
    # stopifnot(all(args2copy %in% names(SIMS)))
    # # runFunArgs <- names(formals(runFun))
    # # compareNames(runFunArgs, args2copy)
    # argsNOT2copy <- setdiff(args2copy, names(formals(runFun)))
    #
    # args2copy <- setdiff(args2copy
    #                      , argsNOT2copy)
    argList_runFun <- extractPlanSpecs(SIMS) #SIMS[args2copy]
    names(argList_runFun)

    argList_runFun <- mergeParameters(
      argList_runFun
      , list(planParms = SIMS$usedParms
             , lastEditParms = lastEditParms
             , planOrigin = planOrigin
             # , locationWeather = locationWeather
             # , drivers_weekData = drivers_weekData
             , drivers = drivers))
    dots <- list(...)
    # .argList_runFun <<- argList_runFun
    # .dots <<- dots
    nullSlot <- names(dots)[sapply(dots, is.null)]
    if (length(nullSlot)){
      log_warn("aphScenarios| dots with value NULL removed:")
      print(nullSlot)
    }
    dots <- dots[!sapply(dots, is.null)]
    # argList_runFun <- .argList_runFun
    # dots <- .dots
    # str(dots)
    log_debug("aphScenario| dotNames: {names(dots)}")
    common <- intersect(names(argList_runFun), names(dots))
    log_debug("aphScenario| common among names(argList_runFun), names(dots) : {names(common)}")
    argList_runFun[common] <- NULL
    argList_runFun[names(dots)] <- dots
    # argList_runFun <- mergeParameters(argList_runFun, dots)
    # print("names(lastEditParms)")
    # print(names(lastEditParms))
  }
  # argList_runFun$planParms
  .argList_runFun00 <<- argList_runFun
  names(.argList_runFun00)
  {
    varyTimeTaken <- system.time({
      scenList <- aphVary(scenDT
                          , FUN = FUN
                          , funArgs = argList_runFun
                          # , targetArg = "lastEditParms" # where scenDT gets pasted in
                          # , includeNames = "auto"
                          , verbosity = 0
      )
    })
    attr(scenList, "varyTimeTaken") <- varyTimeTaken
    print(varyTimeTaken)
    varyList <- list(scenDT = scenDT
                     , argList_runFun = argList_runFun
                     , SIMS = SIMS
                     , scenList = scenList
    )
  }
  varyList
}
