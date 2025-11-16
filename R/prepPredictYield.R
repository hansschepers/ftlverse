#' prepPredictYield
#' better name prepCycle
#' @examples \dontrun{
#' }
#' @export
prepPredictYield <- function(
  DT
  , yoisWeek = aphConstants()$yoisWeek
  , yoisIoT = aphConstants()$yoisIoT
  , bycols = intersect(names(DT), c("plot_syn", "cycle_syn")) # setdiff(aphFactors(DT), aphVariables(DT)) #
  , trim = 0.02
  , correctMissingYield = TRUE
  , voi = "processName"
  , agg2week = TRUE
  # , ...
){
  # dots <- list(...)
  .prepDT <<- copy(DT)
  # DT <- copy(.prepDT)
  
  # DT IoT
  {
    DTiot <- copy(DT)[!processName %in% yoisWeek]
    DTiot <- DTiot[processName %in% yoisIoT]
    
    # DT[, processName := as.character(processName)]
    # getOption("lubridate.week.start")
    #TODO
    # DTiot <- DTiot[, dateTime := toMonday(week(dateTime), weekStart = 2)]
    if (agg2week){
      DTiot[, dateTime := floor_date(dateTime, unit = "week", week_start = 1)]
    } else {
      DTiot[, dateTime := floor_date(dateTime, unit = "day")]
    }
    lubridate::hour(DTiot$dateTime) <- 12
    # DTiot[, percentMonday(dateTime)]
    DTiot
    
    DTiot <- DTiot[, .(value = hmean(value, trim = trim))
                   , by = c(bycols, voi, "dateTime")]
    aphKey(DTiot)
    .DTiot <<- DTiot
    # .DTiot[, percentMonday(dateTime)]
    # pggs(.DTiot)
    # .DTiot
  }
  
  
  # weekdata
  {
    DTweekly <- DT[processName %in% yoisWeek]
    # lubridate::hour(DTweekly$dateTime) <- 12  # necessary if not KBB
    # DTweekly[, weekday := wday(dateTime)]
    # DTweekly[, hr := hour(dateTime)]
    # keys <- aphKey(DTweekly)
    
    DTweekly[, resetDay := NULL]
    columnsToDrop <- setdiff(aphFactors(DTweekly), c(voi, bycols))
    log_trace("Columns dropped: {paste(columnsToDrop, collapse = ', ')}")
    DTweekly[, (columnsToDrop) := NULL]
    
    # htable(DTweekly, voi, "weekday", doplot = F, long = FALSE)
    # htable(DTweekly, voi, "hr", doplot = F, long = FALSE)
    
    # htable(DTweekly, foip, foic, doplot = F, long = FALSE)
    # DTweekly[, hsummary(dateTime), by = bycols]
    #####TODO TODO zzz
    
    # DTweekly <- DTweekly[cycle_syn != "k18"]
    # DTweekly <- DTweekly[cycle_syn != "kbb_18_tuin_3"]
    
    # htable(DTweekly, foip, foic, doplot = F, long = FALSE)
    # DTweekly[, hsummary(dateTime), by = bycols]
    # ppggs(DTweekly, yearSync = 2021, foi = c(foip, foic))
    
    # DTweekly <- DTweekly[!is.na(value)]
    
    # htable(DTweekly, voi, "weekday", doplot = F, long = FALSE)
    # htable(DTweekly, voi, "hr", doplot = F, long = FALSE)
    
    # DTweekly <- DTweekly[hr != 0]
    # ppggs(DTweekly[isoweek(dateTime) %in% 22:25], yearSync = 2021, foi = c(foip, foic))
    
    # DTweekly <- DTweekly[hour(dateTime) != 12]
    # args(htable)
    # htable(DTweekly[, .(weekday = wday(dateTime)), by = c(voi)], voi, "weekday", doplot = F, long = FALSE)
    # DTweekly <- DTweekly[!(hr(dateTime) != 12 & processName == "pruning")]
    # stopifnot(100 == DTweekly[, percentMonday(dateTime)])
  }
  
  # join  week and IoT 
  # DT <- rbind(DTweekly, DTiot)                      # fragile (better?!)
  DT <- rbindlist(list(DTweekly, DTiot), fill = TRUE) # robust
  if (agg2week){
    # print(DTweekly[, percentMonday(dateTime), by = plot_syn])
    # stopifnot(100 == DT[, percentMonday(dateTime)])
    wday(DT$dateTime) <- 2  # same as toMonday()
  }
  # wday(DTwk0$dateTime) <- 2
  hour(DT$dateTime) <- 12
  
  
  # plot names
  if(F){
    DT[, plot_syn := sub("kbb", "k", plot_syn)]
    DT[plot_syn == "k_tuin3_m"   , plot_syn := "k_20_m"]
    DT[plot_syn == "k_tuin3_1053", plot_syn := "k_21_m3"]
    DT[plot_syn == "k_tuin1", plot_syn := "k_21_m1"]
    DT[plot_syn == "k_tuin2", plot_syn := "k_21_m2"]
    DT[plot_syn == "k_tuin3_23", plot_syn := "k_22_m3a"]
    DT[plot_syn == "k_tuin3_24", plot_syn := "k_22_m3b"]
  }
  # DT[, .N, by = plot_syn]
  
  g.DT <<- copy(DT)
  # DT <- copy(g.DT)
  # g.DT[, .N, by = c(voi)]
  stopifnot(nrow(DT) > 0)
  if(!all(DT[, hour(dateTime)] %in% c(0, 12))){
    zzDT00 <<- copy(DT)
    stop("not all on midday")
  }
  
  # DT[, processName := fixFactor(processName)]
  aphKey(DT)
  return(DT)
}
