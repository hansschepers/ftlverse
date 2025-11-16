#' cyclist03Drivers
#' @export
cyclist03Drivers <- function(times = seq(0, 10, .1)
                             , distance.s = 10000*0:2
                             , rawdata = list()
                             , pars.kb
                             , pois_time = pars.kb[xx == "time", unique(simName)]
                             , pois_distance = pars.kb[xx == "distance", unique(simName)]
                             , backwards = 0
                             , doplot = FALSE){
  driversList <- list()
  ##################################### distance ###############################
  {
    # height.s <- ALTITUDE
    # pois_distance <- pars.kb[xx == "distance", unique(simName)]
    pois_distance
    driversList$distance <- data.table(distance = distance.s)
    for (poi in pois_distance){
      
      # str(rawdata)
      if (poi %in% names(rawdata)){
        driversList$distance[, (poi) := rawdata[[poi]]]
      
      } else {
        
        # str(pars.kb)
        with(as.list(pars.kb[simName == poi]), 
             if ( (backwards - reverse) == 0){
               driversList$distance[, (poi) := seq(lb, ub, length.out = length(distance.s))]
             } else {
               driversList$distance[, (poi) := seq(ub, lb, length.out = length(distance.s))]
             }
        )
      }
    }
    if ("hAH" %in% names(driversList$distance)){
      # slope.s <- 0
      driversList$distance[, Slope := c((diff0(hAH) / diff0(distance))[-1], 0)]
    }
  }
  driversList
  
  ################################ time ########################################
  driversList$time <- data.table(time = times)
  # driversList$time[, phase := 0 + (time >= 0)]
  for (poi in pois_time){
    with(as.list(pars.kb[simName == poi]), 
         if ( (backwards - reverse) == 0){
           driversList$time[, (poi) := seq(lb, ub, length.out = length(times))]
         } else {
           driversList$time[, (poi) := seq(ub, lb, length.out = length(times))]
         }
    )
  }
  if (doplot){
    p_dD <- ppggs(aphMelt(driversList$distance, id.vars = "distance")
                  , xoi = "distance", title = "Parameters as function of Distance")
    p_dT <- ppggs(aphMelt(driversList$time, id.vars = "time")
                  , title = "Parameters as function of Time")
    print(p_dD + p_dT)
  }
  .driversList <<- driversList
  driversList
}


#' all_drivers2funs
#' @export
all_drivers2funs <- function(driversList){
  # was dyn_pars
  pois <- list()
  pois$distance <- setdiff(names(driversList$distance), "distance")
  pois$time <- setdiff(names(driversList$time), "time")
  # pois <<- pois
  
  driv_funs <- list()
  driv_funs$distance <- drivers2funs(driversList$distance, xoi = "distance")[pois$distance]
  driv_funs$time     <- drivers2funs(driversList$time    , xoi = "time")[pois$time]
  .driv_funs <<- driv_funs
  driv_funs
}
