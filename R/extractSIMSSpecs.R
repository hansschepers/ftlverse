#' extractSIMSSpecs
#' @examples \dontrun{
#'   SIMStmp <- runFunHES()
#'   #SIMStmp <- SIMS
#'   args2copy <- eval(formals(extractSIMSSpecs)$itemsToExtract)
#'   insims <- names(SIMStmp)
#'   compareNames(args2copy, insims)
#' }
#' @export
extractSIMSSpecs <- function(SIMS = list()
                             , itemsToExtract = c("modelId"
                                                  # , "proj_id"
                                                  # , "doParsOnly"
                                                  # , "doParsDatesOnly"
                                                  # , "proj_nr"
                                                  # , "extraSpecs"
                                                  # , "adminInfo"
                                                  # , "objective_name", "CustomerYield"
                                                  # , "location_name"
                                                  
                                                  # , "continueYield", "continueInits"
                                                  # too big to extract?
                                                  #TODO
                                                  # , "locationWeather"
                                                  # , "drivers_weekData"
                                                  # , "useFromLocationWeather"
                                                  # , "useFromCropSeasonData"
                                                  # , "useGlobalDriverVariables"
                                                  # , "startInSteadyState"
                                                  
                                                  # , "mcParList", "iseed"
                                                  # , "packageVersion.s", "dateDone"
                                                  #, "input", "pfx"
                                                  , "odeMethod"
                                                  , "times"
                                                  , "times_tr"
                                                  , "editList"
                                                  , "lastEditParms"
                                                  # , "daysPerStep"
                                                  # , "keepTimes", "timeModulo"
                                                  # , "usedParms"
                                                  # , "eventList"
                                                  # , "eventListProcessed"
                             )
                             # , customParms = parameterLayers(includePriors = TRUE
                             #                                 , includeInits = TRUE)
                             # , add = c(customParms)
                             # , omit = c("timesSim", "usedParms", "extraSpecs")[0]
                             # , omittedOnPurpose = c("cropLong")
                             , showIgnored = FALSE
){
  # if ("customParms" %in% add) add <- union(add, customParms)
  # if ("customParms" %in% omit) omit <- union(omit, customParms)
  # itemsToExtract <- union(itemsToExtract, add)
  # itemsToExtract <- setdiff(itemsToExtract, omit)
  
  notFound <- setdiff(itemsToExtract, union("", names(SIMS)))
  if (length(notFound)){
    log_warn("missing spec items: {paste(notFound, collapse = ' | ')}")
    itemsToExtract <- setdiff(itemsToExtract, notFound)
  }
  
  if (showIgnored){
    itemsIgnored <- setdiff(names(SIMS), union(itemsToExtract, omittedOnPurpose))
    log_warn("itemsIgnored items: {paste(itemsIgnored, collapse = ' | ')}")
  }
  # print(itemsToExtract)
  SIMSSpecList <- SIMS[itemsToExtract]
  .SIMSSpecList <<- SIMSSpecList
  SIMSSpecList
}
