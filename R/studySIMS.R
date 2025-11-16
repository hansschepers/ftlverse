#' studySIMS
#' @examples \dontrun{
#'   pList <- studySIMS(SIMS)
#'   strList(pList)
#' }
#'
#' @export
studySIMS <- function(SIMS
                      , productId = paste0("plan_", SIMS$plan_id)
                      , topics = c("prettySummary"
                                   , "analyzeParameterLayers"
                                   , "simsPlot"
                                   , "RTR"
                                   , "Fruits"
                                   , "sensPars"
                                   , "sens"
                                   # , "admin"
                      )
                      , sensPars = NULL
                      , focus = c("artifdata", "pb_driver", "pb_crop")
                      , final = c("return", "Reduce", "vrmdAdHoc")[1]
                      , pList = list()
                      , title = SIMS$plan_id
                      , absResid  = TRUE
                      , timesEval = c(35, 70, 105, 140, 175, 210)
                      , minimumSens = 0.05
                      , includeApp = FALSE
){
  if (is.null(sensPars)){
    topics <- setdiff(topics, c("sens", "sensPars"))
  }
  if ("simsPlot" %in% topics){
    pList[["p_simsPlot"]] <- simsPlot(SIMS)
  }
  
  if ("prettySummary" %in% topics){
    pList[["d_prettySummary"]] <- prettySummary(SIMS)
  }
  
  
  if (any(c("sens", "sensPars", "analyzeParameterLayers") %in% topics)){
    ap <- analyzeParameterLayers(SIMS)
    pList[["d_analyzeParameterLayers"]] <- ap
  }
  
  if ("RTR" %in% topics){
    temp_ref <- SIMS$eventListProcessed$temp_ref$value[1]
    if (is.null(temp_ref)){
      temp_ref <- SIMS$usedParms$temp_ref
    }
    pList <- studyTemp(SIMS
                       , pList = pList
                       , temp_ref = temp_ref
    )
  }
  
  if ("admin" %in% topics){
    dtt <- hprettyNum(tail(hdcast(SIMS$cropLong)[, .(Time, local_time
                                                     , setting.pred.cu
                                                     , harvested.fruits.cu)]))
    dtt[, timeTaken := format(SIMS0$timeTaken)["elapsed"]]
    dtt
    dtt[, timeTakenTotal := format(SIMS0$timeTakenTotal)]
    pList[["dt_tail"]] <- dtt
  }
  
  
  if (any(c("sens", "sensPars") %in% topics)){
    pList[["chapterSensitivity Analysis"]] <- 1 
    if (!length(sensPars)){
      sensPars <- trimws(as.data.table(ap)[control == "G", parameter ])
      sensPars <- unique(sub("\\*\\*$", "", sensPars))
      sensPars <- unique(sub("_add$", "", sensPars))
      sensPars <- unique(sub("_mult$", "", sensPars))
      pList[["sensPars"]] <- sensPars
    }
  }
  if ("sens" %in% topics){
    sensList <- sensSIMS(SIMS
                         , par.ois = sensPars
                         , absResid = absResid
                         , timesEval = timesEval
                         , minimumSens = minimumSens)
    pList[["sens"]] <- sensList
    names(sensList$simList$base)
    # vizSensList()
    p_sensList <- vizSensList(sensList
                              , focus = focus
                              , extraYois = "growthRatePercentage"
                              , includeApp = includeApp)
    pList <- c(pList, p_sensList)
  }
  
  names(pList)
  
  if ("vrmdAdHoc" %in% final){
    vrmdAdHoc(productId = paste0("plan", SIMS$plan_id)
              , p_List = pList#[3:4]
    )
  }
  if ("return" %in% final){
    return(pList)
  }
  if ("Reduce" %in% final){
    p <- Reduce("+", pList)
    return(p)
  }
}
