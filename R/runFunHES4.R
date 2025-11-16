#' runFunHES4
#' 
#' @examples \dontrun{
#'   rmDotObjects() ; SIMS <- runFunHES4(FUNODE = cyclist04)
#'   summary(SIMS)
#' }
#' 
#' @export
runFunHES4 <- function(modelId = "cyclist03"
                      , times
                      , times_tr
                      , driversList = list()
                      , editList = list()
                      , CONSTANTS = list()
                      , context = c("tempsweepup", "huez")[2]
                      , rawdata
                      # parameters to vary in drivers
                      , pars.kb
                      , pois_distance
                      , pois_time
                      # 
                      , FUNODE# = cyclist04
                      , FUNPARMS
                      , FUNSTATES
                      , FUNDRIVERS
                      , Pars
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
){
  if (!is.null(session)){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "getting Context, Parameters, Drivers ...", value = 0)
  }
  ########################################## start context parsing
  CONTEXT <- getContext(context)[[context]]
  # str(CONTEXT)
  
  if (missing(times)) times <- CONTEXT$times
  if (missing(times_tr)) times_tr <- times
  if (missing(pars.kb)) {
    pars.kb <- CONTEXT$pars.kb
    .pars.kb <<- pars.kb
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
    FUNODE <- get(modelId)
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
  print(parmsUsed[c("T_air", "hAH", "Slope", "brainpush")])
  .parmsUsed <<- parmsUsed
  
  ################# put in global...: driv_funs    pois    all elements of CONSTANTS
  list2env(CONSTANTS, envir = .GlobalEnv)
  
  if (!missing("out")) {
    y_prior_last <- setNames(as.vector(tail(out[, namesOutputVariables], 1))
                             , namesOutputVariables)
  }
  
  y_init00 <- unlist(FUNSTATES())
  if (missing(y_init)){
    if (!missing("out")) {
      log_info("using last state of provided run")
      y_init <- y_prior_last
    } else {
      y_init <- unlist(y_init00)
      # y_init <- c(CONSTANTS$TSET, c(x = 0, v = 0))
    }
  }
  namesOutputVariables <- names(y_init)
  # y_init
  # print(hprettyNum(y_init))
  
  ##############################################################################
  pois_distance_s <- pars.kb[xx == "distance", simName]
  pois_time_s <- pars.kb[xx == "time", simName]
  
  missing_pois_distance <- setdiff(pois_distance, pois_distance_s)
  if (length(missing_pois_distance)) log_warn("missing pars for distance drivers: {missing_pois_distance}")
  pois_distance <- intersect(pois_distance, c(pois_distance_s, names(rawdata)))
  .pois_distance <<- pois_distance
  
  missing_pois_time_s <- setdiff(pois_time, pois_time_s)
  if (length(missing_pois_time_s)) log_warn("missing pars for time drivers: {missing_pois_time_s}")
  pois_time <- intersect(pois_time, pois_time_s)
  .pois_time <<- pois_time
  
  log_info("pois_distance: {pois_distance}")
  log_info("pois_time: {pois_time}")
  
  timeTaken <- 0
  iitime <<-  0 ; iiggtime <<- 0
  iiggdata <<- data.table(ind = 1:1e5, timeCalc = NA_real_)
  
  if (!is.null(session)){
    progress$set(message = "Simulating", value = 0.1)
  }
  ############################## run transient ##############################
  if(doTransient){
    timeTaken_tr <- system.time({
      log_info("running Transient")
      message("y_init before transient")
      y_init_tr <- y_init
      print(hprettyNum(y_init))
      driversList_tr <- FUNDRIVERS(times = times
                                   , backwards = 1
                                   , rawdata = rawdata
                                   , pars.kb = pars.kb
                                   , pois_distance = pois_distance
                                   , pois_time = pois_time)
      .driversList
      driv_funs <- all_drivers2funs(driversList_tr)
      driv_funs <<- driv_funs
      # pois
      out_tr <- deSolve::ode(y = y_init
                             , times = times_tr, func = FUNODE, parms = parmsUsed
                             , rtol = rtol, atol = atol, hmax = hmax_value
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
      print(timeTaken_tr)
    })
  }
  
  ############################## run with new drivers ########################
  timeTaken <- system.time({
    message("y_init after transient")
    print(hprettyNum(y_init))
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
    driv_funs <<- all_drivers2funs(driversList)
    # pois
    out <- deSolve::ode(y = y_init
                        , times = times, func = FUNODE, parms = parmsUsed
                        , rtol = rtol, atol = atol, hmax = hmax_value
                        , method = odeMethod)
    # p_Temp <- plotBySegment(out, dois = doi, layerOUT = "whole body")
    # p_Temp
    # vrmd("add", p = p_Temp, chunkId = "Temperature sweep")
  })
  print(timeTaken)
  iiggdata <- iiggdata[!is.na(timeCalc)]
  log_info("Times that velocity was negative: {ii_negative_time} at time = {round(timeOfLastNegativeAccelleration, 3)}")
  log_info("calls to model: {iiggtime}")
  log_info("seconds per 1000 calls {round(1e3 * timeTaken[[1]] / iiggtime, 3)}")
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
  
  SIMS <- mget(ls())
  class(SIMS) <- c("SIMS", "list")
  return(SIMS)
}
