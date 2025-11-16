#' runFunHES
#' 
#' @examples \dontrun{
#'   log_threshold(INFO)
#'   rmDotObjects() ; SIMS00 <- runFunHES(editList = list(test = 101))
#'   summary(SIMS00)
#'   log_threshold(DEBUG)
#'   rmDotObjects() ; SIMS01 <- runFunHES(input = SIMS00)
#'   summary(SIMS01)
#'   SIMS01$editList
#'   SIMS01$lastEditParms
#' }
#' 
#' @export
runFunHES <- function(modelId = "cyclist03"
                      , input = list()
                      , doParsOnly = FALSE
                      , ignoreFromInput= c("")[0]
                      , times
                      , times_tr
                      , driversList = list()
                      , editList = list()
                      , lastEditParms = list()
                      , CONSTANTS = list()
                      # , CONSTANTSname
                      , context = c("tempsweepup", "huez")[2]
                      , rawdata
                      # parameters to vary in drivers
                      , pars.kb
                      , pois_distance
                      , pois_time
                      # 
                      , FUNODE# = cyclist05
                      , FUNPARMS
                      , FUNSTATES
                      , FUNDRIVERS
                      , Pars
                      , driv_funs = list(distance = list(), time = list())
                      # y(t=0)
                      , y_init
                      , out
                      # transient
                      , doTransient = FALSE
                      # ODE solver
                      , odeMethod = c("lsoda", "rk4")[1]
                      , hmax_value = 0.02
                      , rtol = .01
                      , atol = c(rep(0.1, 25), c(100, 200))
                      , session = NULL
                      , toReturn = "all"
                      , notToReturn = c("func"
                                        # , "cropLong", "Time"
                                        # , "StatesFUN"
                                        # , "tmp1", "tmp2", "lastNonNAValue"
                                        , "toReturn"
                                        # , "PARS", "usedArguments"
                                        # , "make_dates_stuff_Args", "StatesFUN"
                                        # , "dates_stuff_list"
                                        # , "new_drivers_list", "weatherList"
                                        # , "drivers_weekData"
                                        , "funFormalsNames", "nm", "CALL", "input")
){
  # str(session)
  if (!is.null(session)){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "getting Context, Parameters, Drivers ...", value = 0)
  }
  
  
  ################################################## inputs ############
  CALL <- match.call()
  CALLnames <- union(names(CALL), ignoreFromInput)
  # str(CALLnames)
  
  if (exists("g.runFunInput") & !length(input)){
    log_debug("using global object 'g.runFunInput'. .")
    input <- g.runFunInput
  }
  .input0000 <<- input
  if (inherits(input, "SIMS") | inherits(input, "SIMSPARS")){
    input <- extractSIMSSpecs(input)
  }
  .input00 <<- input
  inputNames <- names(input)
  inputsChecked <- inputParserHES(input = input
                               , FUN = "runFunHES"
                               , CALLnames = CALLnames
                               , pos = 0
                               , verbosity = verbosity)
  .inputsChecked <<- inputsChecked
  list2env(inputsChecked, environment())
  # for(nm in names(inputsChecked)) assign(nm, inputsChecked[[nm]])
  # str(goFast)
  
  
  ########################################## start context parsing
  CONTEXT <- getContext(context, KB_LIST = readKB_LIST(doAttach = T))[[context]]
  .CONTEXT <<- CONTEXT
  # str(CONTEXT)
  
  if (missing(times)) times <- CONTEXT$times
  if (missing(times_tr)) times_tr <- times
  if (missing(pars.kb)) {
    pars.kb <- CONTEXT$pars.kb
    .pars.kb <<- pars.kb
  }
  if (is.null(pars.kb)) {
    pars.kb <- data.table(xx = "", simName = "")
  }
  if (missing(rawdata)){
    rawdata <- CONTEXT$rawdata
  }
  if (missing(pois_distance)){
    pois_distance <- CONTEXT$pois_distance
  }
  if (missing(pois_time)){
    pois_time <- CONTEXT$pois_time
  }
  ########################################## end context parsing
  
  if (missing(FUNODE)) {
    stopifnot(exists("cyclist05"))
    FUNODE <- cyclist05
    # FUNODE <- get(modelId)
  }
  if (missing(FUNPARMS)) FUNPARMS <- get(paste0(modelId, "Parms"))
  if (missing(FUNSTATES)) FUNSTATES <- get(paste0(modelId, "States"))
  if (missing(FUNDRIVERS)) FUNDRIVERS <- get(paste0(modelId, "Drivers"))
  ############################################################################## CONSTANTS
  if (!length(CONSTANTS)) {
    CONSTANTS <- get(paste0(modelId, "Constants"))()  ## !! with () !!
  }
  # {
  #   parTables <- makeTables(CONSTANTS, segment.s)
  #   parTables$dcPars6
  # }
  
  ############################################################################## PARMS    
  if (missing(Pars)) {
    Pars <- FUNPARMS()
  }
  # print(Pars[c("T_air", "hAH", "Slope", "brainpush")])
  
  
  parmsUsed <- as.list(Pars)
  parmsUsed <- unlist(mergeParameters(as.list(parmsUsed), driversList))
  parmsUsed <- unlist(mergeParameters(as.list(parmsUsed), editList))
  parmsUsed <- unlist(mergeParameters(as.list(parmsUsed), lastEditParms))
  # print(parmsUsed[c("T_air", "hAH", "Slope", "brainpush")])
  .parmsUsed <<- parmsUsed
  
  ################# put in global...: driv_funs    pois    all elements of CONSTANTS
  # list2env(CONSTANTS, envir = .GlobalEnv)
  
  if (!missing("out")) {
    y_prior_last <- setNames(as.vector(tail(out[, namesOutputVariables], 1))
                             , namesOutputVariables)
  }
  
  y_init00 <- unlist(FUNSTATES())
  if (missing(y_init)){
    if (!missing("out")) {
      log_debug("using last state of provided run")
      y_init <- y_prior_last
    } else {
      y_init <- unlist(y_init00)
      # y_init <- c(CONSTANTS$TSET, c(x = 0, v = 0))
    }
  }
  namesOutputVariables <- names(y_init)
  fromEditList <- intersect(namesOutputVariables, sub("\\.init$", "", names(editList)))
  # str(fromEditList)
  if (length(fromEditList)){
    log_warn("taking values from editList into y_init: {paste(fromEditList, collapse = ' ')}")
    y_init[fromEditList] <- editList[paste0(fromEditList, ".init")]
    y_init <- unlist(y_init)
  }
  # str(y_init)
  # print(hprettyNum(y_init))
  
  ##############################################################################
  pois_distance_s <- pars.kb[xx == "distance", simName]
  pois_distance_s <- union(pois_distance_s, setdiff(names(rawdata), c("distance", "time")) )
  pois_time_s <- pars.kb[xx == "time", simName]
  pois_time_s <- union(pois_time_s, setdiff(names(rawdata), c("distance", "time")) )
  
  missing_pois_distance <- setdiff(pois_distance, pois_distance_s)
  if (length(missing_pois_distance)) log_warn("missing pars for distance drivers: {missing_pois_distance}")
  pois_distance <- intersect(pois_distance, c(pois_distance_s, names(rawdata)))
  .pois_distance <<- pois_distance
  
  missing_pois_time_s <- setdiff(pois_time, pois_time_s)
  if (length(missing_pois_time_s)) log_warn("missing pars for time drivers: {missing_pois_time_s}")
  pois_time <- intersect(pois_time, pois_time_s)
  .pois_time <<- pois_time
  
  log_debug("pois_distance: {pois_distance}")
  log_debug("pois_time: {pois_time}")
  
  
  if (doParsOnly) {
    # message(282)
    parsDateDone <- Sys.time()
    toReturn <- ls()
    toReturn <- union(c("modelId"  # MUST stay at position 1!
                        , "usedParms")
                      , setdiff(toReturn, notToReturn))
    SIMSPARS <- structure(mget(toReturn), class = c("list", "SIMSPARS"))
    return(SIMSPARS)
  }
  
  
  
  # timeTaken <- 0
  iitime <<- 1 ; iiggtime <<- 1
  iiggdata <<- data.table(ind = 1:1e5, timeCalc = NA_real_)
  
  if (!is.null(session)){
    progress$set(message = "Simulating", value = 0.1)
  }
  ############################## run transient ##############################
  if(doTransient){
    # timeTaken_tr <- system.time({
    log_debug("running Transient")
    message("y_init before transient")
    y_init_tr <- y_init
    # print(hprettyNum(y_init))
    driversList_tr <- FUNDRIVERS(times = times_tr
                                 , backwards = 1
                                 , rawdata = rawdata
                                 , pars.kb = pars.kb
                                 , pois_distance = pois_distance
                                 , pois_time = pois_time)
    .driversList
    driv_funs <- all_drivers2funs(driversList_tr)
    .driv_funs_tr <<- driv_funs
    # pois
    out_tr <- deSolve::ode(y = y_init
                           , times = times_tr
                           , func = FUNODE
                           , parms = parmsUsed
                           , rtol = rtol, atol = atol, hmax = hmax_value
                           , driv_funs = driv_funs
                           , CONSTANTS = CONSTANTS
                           , method = odeMethod)
    y_init <- setNames(as.vector(tail(out_tr[, namesOutputVariables], 1))
                       , namesOutputVariables)
    y_init["x"] <- 0
    out_tr <- as.data.table(out_tr)
    # p_Temp_tr <- plotBySegment(out_tr, dois = doi, layerOUT = "whole body")
    # p_Temp_tr
    if (!is.null(session)){
      progress$set(value = 0.3, detail = "transient done...")
    }
    # })
    # print(timeTaken_tr)
  }
  
  ############################## run with new drivers ########################
  # timeTaken <- system.time({
  log_debug("y_init after transient")
  # print(hprettyNum(y_init))
  driversList <- FUNDRIVERS(times = times
                            , backwards = 0
                            , distance.s = rawdata$distance
                            , rawdata = list(hAH = rawdata$hAH)
                            , pars.kb = pars.kb
                            , pois_distance = pois_distance
                            , pois_time = pois_time)
  driversList
  # driversList <- FUNDRIVERS(times = times
  #                           , backwards = 0
  #                           , pars.kb = pars.kb
  #                           , pois_distance = pois_distance
  #                           , pois_time = pois_time)
  # .driversList
  driv_funs <- all_drivers2funs(driversList)
  .driv_funs <<- driv_funs
  # str(times)
  # pois
  out <- deSolve::ode(y = y_init
                      , times = times, func = FUNODE, parms = parmsUsed
                      , rtol = rtol, atol = atol, hmax = hmax_value
                      , driv_funs = driv_funs
                      , CONSTANTS = CONSTANTS
                      , method = odeMethod)
  # p_Temp <- plotBySegment(out, dois = doi, layerOUT = "whole body")
  # p_Temp
  # })
  # print(timeTaken)
  iiggdata <- iiggdata[!is.na(timeCalc)]
  log_debug("Times that velocity was negative: {ii_negative_time} at time = {round(timeOfLastNegativeAccelleration, 3)}")
  log_debug("calls to model: {iiggtime}")
  # log_debug("seconds per 1000 calls {round(1e3 * timeTaken[[1]] / iiggtime, 3)}")
  if (!is.null(session)){
    progress$set(value = 0.8, detail = "done...")
  }
  
  y_last <- setNames(as.vector(tail(out[, namesOutputVariables], 1))
                     , namesOutputVariables)
  out <- as.data.table(out)
  rm(list = c("FUNODE", "FUNPARMS", "FUNSTATES", "FUNDRIVERS"))
  # FUNODE <- sapply(body(FUNODE), as.character, simplify = F)
  # FUNPARMS <- sapply(body(FUNPARMS), as.character, simplify = F)
  # FUNDRIVERS <- sapply(body(FUNDRIVERS), as.character, simplify = F)
  # FUNSTATES <- sapply(body(FUNSTATES), as.character, simplify = F)

  if ("all" %in% toReturn) {
    log_trace("return all elements currently present")
    toReturn <- ls()
  } else {
    if (!"artifDat" %in% toReturn) {
      toReturn <- intersect(toReturn, ls())
    }
  }
  toReturn <- union(c("modelId"  # MUST stay at position 1!
                      # , "df_kpi", "packageVersion.s", "sessionI"
                      # , "dateDone", "timeStarted", "timeTakenTotal", "timeTaken"
                      # , "cropLong", "usedParms", "drivers"
                      )
                    , setdiff(toReturn, notToReturn))

  SIMS <- structure(mget(toReturn), class = c("list", "SIMS"))
  .SIMS <<- SIMS
  return(SIMS)
}
