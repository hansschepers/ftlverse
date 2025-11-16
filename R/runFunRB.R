#' runFunRB
#' 
#' @examples \dontrun{
#'   library(logger)
#'   log_threshold(INFO)
#'   log_threshold(DEBUG)
#'   source("global.R") ; openScreen()
#'   rmDotObjects() ; RB_SIMS00 <- runFunRB(editList = list(IR = 100))
#'   pggs(aphMelt(RB_SIMS00$out, fois = "gc"), logx = TRUE)
#'   ppggs(RB_SIMS00$out, xoi = "gc", yoi = "M_c", logx = T)
#'   RB_SIMS00$out
#'   unlist(RB_SIMS00$parmsUsed)
#'   #
#'   rmDotObjects() ; RB_SIMS00 <- runFunRB(editList = list(IR = 100, TT = 3)
#'                                        , odeMethod = "lsoda", hmax_value = 0.002, rtol = .001, atol = 0.001
#'                                        , y_init = c(r = NA)
#'                                        )
#'   RB_SIMS00$out
#'   pggs(aphMelt(RB_SIMS00$out))
#' }
#' 
#' @export
runFunRB <- function(modelId = "cyclist03"
                     , input = list()
                     , doParsOnly = FALSE
                     , ignoreFromInput= c("")[0]
                     , driversList = list()
                     , editList = list()
                     , lastEditParms = list()
                     , CONSTANTS = list()
                     # , CONSTANTSname
                     , context = c("recbox", "hiit")[2]
                     # , rawdata
                     # parameters to vary in drivers
                     # , pars.kb
                     # , pois_distance
                     # , pois_time
                     # 
                     , FUNODE = recbox01ODE
                     # , FUNPARMS
                     # , FUNSTATES
                     # , FUNDRIVERS
                     # chart A-1 on page 69
                     , Pars = recbox01Parms()
                     , times = NULL
                     , ncycles = 5
                     , length.out = 1001
                     # , times_tr
                     , driv_funs = list(distance = list(), time = list())
                     # y(t=0)
                     , y_init = c(r = 1)[0]
                     , timeSeries = FALSE
                     , out = data.table()
                     # transient
                     # , doTransient = FALSE
                     # ODE solver
                     , odeMethod = c("lsoda", "rk4")[2]
                     , hmax_value = 0.02
                     , rtol = .01
                     , atol = 0.001
                     , session = NULL
                     , toReturn = "all"
                     , notToReturn = c("func"
                                       , "FUNODE", "FUNPARMS", "FUNSTATES", "FUNDRIVERS"
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
  
  
  ############################################################################## PARMS    
  # if (missing(Pars)) {
  #   Pars <- FUNPARMS()
  # }
  
  parmsUsed <- as.list(Pars)
  # log_info("parms init")
  # print(hprettyNum(parmsUsed))
  parmsUsed <- mergeParameters(as.list(parmsUsed), editList)
  # log_info("before CONSTANTS merged")
  # print(hprettyNum(parmsUsed))
  .CONSTANTS <<- CONSTANTS
  # CONSTANTS <- .CONSTANTS
  parmsUsed <- mergeParameters(as.list(parmsUsed), CONSTANTS$stimulus)
  parmsUsed <- mergeParameters(as.list(parmsUsed), CONSTANTS$kinetics)
  parmsUsed <- mergeParameters(as.list(parmsUsed), CONSTANTS$activities)
  # log_info("after CONSTANTS merged")
  # print(hprettyNum(parmsUsed))
  parmsUsed <- mergeParameters(as.list(parmsUsed), lastEditParms)
  # print(parmsUsed[c("T_air", "hAH", "Slope", "brainpush")])
  .parmsUsed <<- parmsUsed
  kpis <- recbox01List(parmsUsed)
  .kpis <<- kpis
  
  if (doParsOnly) {
    # message(282)
    parsDateDone <- Sys.time()
    toReturn <- ls()
    toReturn <- union(c("modelId"  # MUST stay at position 1!
                        , "parmsUsed")
                      , setdiff(toReturn, notToReturn))
    SIMSPARS <- structure(mget(toReturn), class = c("list", "SIMSPARS"))
    return(SIMSPARS)
  }
  
  
  # timeTaken <- 0
  if (!is.null(session)){
    progress$set(message = "Simulating", value = 0.1)
  }
  
  if (!length(times)){
    times <- seq(0, ncycles*parmsUsed$TT, length.out = length.out)
  }
  
  ################################################################
  if (length(y_init)){
    
    if (is.character(y_init)){
      y_init <- kpis[[y_init]]
      if (is.null(y_init)) {
        log_warn("kpi as y_init not found, taking y(0) = 1")
        y_init <- 1
      }
      y_init <- setNames(y_init, "r")
    }
    if (!length(times)){
      times = seq(0
                  , 5*parmsUsed$TT
                  , length.out = 1001)
    }
    
    driversList <- recbox01Drivers(times = times
                                   , parms = parmsUsed)
    driversList
    driv_funs <- all_drivers2funs(driversList)
    # driv_funs$time$gt(times)
    FUNODE = recbox01ODE
    out <- deSolve::ode(y = y_init
                        , times = times
                        , func = FUNODE
                        , parms = parmsUsed
                        , driv_funs = driv_funs
                        , rtol = rtol, atol = atol
                        , hmax = hmax_value
                        , method = odeMethod)
    out <- as.data.table(out)
    out0 <<- copy(out)
    # out <- copy(out0)
    # out <- copy(.RB_SIMST$out)
    # post processing
    out[, stimulus := driv_funs$time$gt(times)]
    out[, A_r := with(parmsUsed, {
      gt <- stimulus
      ar <- (a1 + a2 * gt)      / (1 + gt)
      br <- (a4 + a3 * gt * cc) / (1 + gt * cc)
      A_r <- ar * r + br * (1 - r)
      A_r})]
    out
    kpit <- recbox01List(parmsUsed
                         , time = out$time
                         , y = out$r)
    
    ################################################################
  } else {
    ################################################################
    
    out <- recbox01Alg(out = out
                       , parms = parmsUsed
                       , timeSeries = FALSE)
  }
  ################################################################
  
  if ("all" %in% toReturn) {
    log_trace("return all elements currently present")
    toReturn <- ls()
  } else {
    if (!"artifDat" %in% toReturn) {
      toReturn <- intersect(toReturn, ls())
    }
  }
  toReturn <- union(c("modelId")
                    , setdiff(toReturn, notToReturn))
  
  SIMS <- structure(mget(toReturn), class = c("list", "SIMS"))
  .SIMS <<- SIMS
  return(SIMS)
}
