#' trapro
#' @examples \dontrun{
#'   dtIn <- data.table(processName = c("temp_air", "aunknown", "GHTEMP"))
#'   trapro(dtIn)
#'   trapro(dtIn$processName)
#'   .Last_traTable
#'   trapro(dtIn, "sim2displayNL")
#'   trapro(dtIn, "sim2data")
#'   dtIn |> trapro("sim2data") |> trapro("data2sim")
#'
#'   # chaining: 2 step
#'   dtIn |> trapro("data2sim") |> trapro("sim2displayEN")
#'   # one step is different!
#'   dtIn |> trapro("data2displayEN")
#'
#'   trapro(dtIn$processName, "data2sim") |> trapro("sim2displayEN")
#'   trapro(trapro(dtIn$processName, "data2sim"), "sim2displayEN")
#'
#'   trapro(c("temp_air", "aunknown"), "sim2data")
#'   trapro(c("temp_air", "aunknown"), "sim2displayEN")
#'   xx <- trapro(c("temp_air", "aunknown"), "sim2displayEN", doOrdered = TRUE)
#'   levels(xx)
#'   xx
#' }
#' @export
trapro <- function(dt
                   , traTable = c("sim2displayEN", "sim2displayNL", "sim2displayXX"
                                  , "sim2data", "data2sim"
                                  , "data2displayNL", "data2displayEN")[1]
                   , extraDict = NULL
                   , doOrdered = FALSE
                   , KB_LIST = NULL
                   , variable.name = "processName"
                   , doAutoAttachFirstKB_LIST = TRUE
){
  if (!length(traTable)) return(dt)
  
  if (inherits(dt, "data.frame")){
    variable.name = intersect(c("processName", "variable"), names(dt))[1]
    # str(variable.name)
    log_trace("variable.name {variable.name}")
    if (!length(variable.name)) {
      log_error("no 'processName' or 'variable' found in data table?")
      return(dt)
    }
  }
  
  if (is.null(names(traTable)[1])){
    if (length(traTable) > 1){
      message("traTable not length 1:")
      print(traTable)
    }
    if(!exists(traTable)){
      log_debug("trapro| object {traTable} not found, reading KB_LIST...")
      if (is.null(KB_LIST)){
        KB_LIST <- readKB_LIST(doAttach = doAutoAttachFirstKB_LIST)
        traTable <- KB_LIST[[traTable]]
      } else {
        if ("HES_KB_LIST" %in% search()){
          traTable <- get(traTable, "HES_KB_LIST")
        } else {
          stop("traTable not found")
        }
      }
    } else {
      log_trace("using existing translationTable {traTable}")
      traTable <- get(traTable)
    }
  }
  traTable <- c(extraDict, traTable)
  .Last_traTable <<- traTable
  
  
  if (inherits(dt, "data.frame")){
    if (variable.name %in% names(dt)){
      .dtTrapro <<- dt
      setDT(dt)
      dt <- copy(dt)
      dt[, (variable.name) := trapro(get(variable.name)
                                     , traTable = traTable
                                     , doOrdered = doOrdered)]
      return(dt[])
      # } else {
      #   log_error("no 'processName' or 'variable' found in data table?")
    }
  }
  
  
  if (inherits(dt, "list")){
    log_debug("trying to translate a list... fingers crossed, to be tested")
    lapply(dt, trapro, traTable = traTable, doOrdered = doOrdered)
  }
  
  # str(traTable)
  # dt <- dt$processName
  if (is.factor(dt) | is.character(dt)){
    wasFactor <- is.factor(dt)
    wasOrdered <- is.ordered(dt)
    
    sumNAbefore <- sumna(dt)
    if (sumNAbefore > 0){
      message("NA's before translation! {sumNAbefore}")
    }
    
    dt <- as.character(dt)
    # compareNames(as.character(dt), names(traTable))
    found <- as.character(dt) %in% names(traTable)
    # table(found)
    dt[found] <- traTable[dt[found]]
    if (wasOrdered) dt <- as.factor(dt)
    if (wasFactor | doOrdered) dt <- factor(dt, levels = unique(dt), ordered = TRUE)
    
    sumNAafter <- sumna(dt)
    if (sumNAafter > 0){
      message("NA's after translation! {sumNAafter}")
    }
    
    return(dt)
  }
}
