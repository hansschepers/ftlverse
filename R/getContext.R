#' getContext
#' 
#' @export
getContext <- function(context = "sweep"
                       , pois = "T_air"
                       , contextParms = list(brainpush = .15)
                       , KB_LIST){
  if (missing(KB_LIST)){
    log_debug("getContext| KB_LIST missing")
    if (!exists("KB_LIST")){
      log_debug("getContext| reading KB_LIST AND attaching")
      KB_LIST <- readKB_LIST(doAttach = TRUE)
    }
  }
  context <- tolower(context)
  res <- list()
  pars.kb <- KB_LIST$KBDB[type == "pars"]
  pars.kb
  
  if ("baserun6min" %in% context){
    pois_time <- character()
    pois_distance <- character()
    res[["baserun6min"]] <- list(
      pars.kb = data.table(xx = "", simName = "")
      , rawdata = data.table()
      , pois_distance = character()
      , pois_time = character()
      , times = seq(0, .1, .0025)
    )
  }
  
  if ("baserun2hr" %in% context){
    pois_time <- character()
    pois_distance <- character()
    res[["baserun2hr"]] <- list(
      pars.kb = data.table(xx = "", simName = "")
      , rawdata = data.table()
      , pois_distance = character()
      , pois_time = character()
      , times = seq(0, 2, .05)
    )
  }
  
  if ("huez" %in% context){
    pois_time <- character()
    pois_distance <- "hAH"
    res[["huez"]] <- list(
      pars.kb = pars.kb[simName %in% pois_time]
      , rawdata = KB_LIST[["huez"]]
      , pois_distance = pois_distance
      , pois_time = pois_time
      , times = seq(0, 0.25, .005)
    )
  }
  
  
  if (any(c("sweep", "all") %in% context)){
    pois_distance <- character()
    pois_time <- pois
    res[["sweepup"]] <- list(
      pars.kb = pars.kb[simName %in% pois_time]
      , rawdata = data.table()
      , contextParms = contextParms
      , pois_distance = pois_distance
      , pois_time = pois_time
      , times = seq(0, 50, 0.5)
    )
  }
  
  if (any(c("tempsweepup", "tempsweepdown") %in% context)){
    pois_distance <- character()
    pois_time <- "T_air"
    res[["tempsweepup"]] <- list(
      pars.kb = pars.kb[simName %in% pois_time]
      , rawdata = data.table()
      , contextParms = list(brainpush = .15)
      , pois_distance = pois_distance
      , pois_time = pois_time
      , times = seq(0, 50, 0.5)
    )
  }
  
  if ("tempsweepdown" %in% context){
    res$tempsweepdown <- res$tempsweepup
    res$tempsweepdown$pars.kb[simName %in% res$tempsweepdown$pois_time, reverse := 1][]
    res$tempsweepup <- NULL
  }
  
  res
}



#' setupExperiment
#' 
#' context is a generic description of use case (lab_test, outdoor_race, ...)
#' details are overlaid as 'experiment' with setupExperiment()
#' protocol
#' 
#' @examples \dontrun{
#'   getContext()
#'   getContext("huez")
#'   experimentList <- getContext("sweep", pois = "T_air")
#'   experimentList
#'   experimentList <- setupExperiment(experimentList, pars_to_reverse = "T_air")
#' }
#' 
#' @export
setupExperiment <- function(experimentList
                            , experimentNames = names(experimentList)
                            , pars_to_reverse = "T_air"
                            , protocolEdit = "sweep"
                            , reverse = character()
                            , ...){
  for (poiRev in pars_to_reverse){
    for (nm in experimentNames){
      experimentList[[nm]]$pars.kb[simName %in% poiRev, reverse := 1][]
    }
  }
  experimentList <- mergeParameters(experimentList, ...)
  .experimentList <<- experimentList
  experimentList
}
