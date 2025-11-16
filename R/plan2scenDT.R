#' makescenPlan
#' @example inst/example/e_planCombi.R
#' @export
makescenPlan <- function(variedParameters = character()
                         , paramGroup = character()
                         , parmsOut = c("continueTemp_sum")
                         , ranges = NULL
                         , steps = 11
                         , multLB = .5, multUB = 1.5
                         , KBDB = NULL){
  if (length(paramGroup)){
    parSets <- getParSets()
    stopifnot(paramGroup %in% names(parSets))
    parNames <- names(parSets[[paramGroup]])
    variedParameters <- union(variedParameters, parNames)
  }

  variedParameters <- setdiff(variedParameters, parmsOut)
  if (length(variedParameters) == 0){
    variedParameters <- names(ranges)
  }
  stopifnot(length(variedParameters) > 0)

  if (!length(ranges)){
    log_debug("getting parameter ranges from KBDB")
    if (is.null(KBDB)){
      KBDB = readKBDB()
    }

    toVary <- KBDB[simName %in% variedParameters, .(simName, lb, ub)]
    if (log_threshold() >= 500) {str(toVary)}

    ranges <- sapply(variedParameters, \(parName){
      log_debug(parName)
      toUse <- toVary[simName == parName]

      if (nrow(toUse) == 0){
        log_debug("hello {parName}")
        if (grepl("_mult$", parName, ignore.case = TRUE)){
          log_debug("_mult defaulting to {multLB}, {multUB}")
          return(list(lb = multLB, ub = multUB))
        } else {
          if (grepl("_add$", parName, ignore.case = TRUE)){
            log_warn("not yet possible _add in KBDB defaulting")
            return(NULL)
          }
        }
      }
      # str(toUse)
      if (nrow(toUse) != 1){
        log_error("not just one entry for {parName}: {nrow(toUse)}")
      }
      as.list(toUse[1])
    }
    , simplify = FALSE, USE.NAMES = TRUE)
  }

  # .ranges <<- ranges
  # ranges <- as.data.table(ranges)
  scenPlan <- sapply(variedParameters, \(parName){
    with(ranges[[parName]]
         , seq(from = lb, to = ub, length.out = steps))}
    , simplify = FALSE, USE.NAMES = TRUE)

  scenPlan
}




# scenPlan1 <- list(lue_ref = c(2, 3)
#                   , temp_ref = seq(16.5, 20.5, 2)
#                   , RTR = seq(0, 4, 1)
#                   # , plantingWeekShift = c(-2, 0, 2)
#                   # , stemDensityChangeStart = seq(6, 11, 1)
# )
# # scenPlan1 <- list(fw_maxPlasticity = c(0.03, 0.08))
# # scenPlan1 <- list(settingSuccess.sens = c(0.5, .2))
# # scenPlan1 <- list(Strength_reqSetting = c(40, 200))
# # scenPlan2 <- list(plantingWeekShift = seq(0, 50, 10))
# # scenPlan2 <- list(stemDensityStart = seq(1, 9, 1))
# scenPlan2 <- list(trussSpeed_ref = seq(.6, .9, .1))
# scenDT1 <- plan2scenDT(scenPlan1, type = "star", doAddBase = F)
# scenDT2 <- plan2scenDT(scenPlan2, type = "ribs", doAddBase = F)
# scenDT1
# scenDT2
# setnames(scenDT1, "scenId", "scenId1")
# setnames(scenDT2, "scenId", "scenId2")
# scenDTcombi <- CJ(scenId1 = seq(nrow(scenDT1)), scenId2 = seq(nrow(scenDT2)))
# scenDTcombi <- scenDT2[scenDT1[scenDTcombi, on = "scenId1"], on = "scenId2"]
# scenDTcombi[, scenId := seq(nrow(scenDTcombi))]
# scenDTcombi[, scenId1 := NULL]
# scenDTcombi[, scenId2 := NULL]
# scenDTcombi



#' plan2scenDT
#' @examples \dontrun{
#'   scenPlan <- makescenPlan(variedParameters = c("lue_ref", "RTR", "temp_ref")
#'                              , steps = 4)
#'   scenDT <- plan2scenDT(scenPlan, "star", doAddBase = TRUE)
#'
#'   scenDT <- plan2scenDT(scenPlan, "ribs", doAddBase = TRUE)
#'   scenDT
#'
#'   scenDT <- plan2scenDT(scenPlan, "sides", doAddBase = TRUE)
#'   scenDT <- plan2scenDT(scenPlan, "crossjoin", doAddBase = FALSE)
#'
#'   scenDT <- fillScenDT(scenDT, pars = lapply(scenPlan, mean))
#'   plotScenPlan(scenPlan, scenDT)
#' }
#' @importFrom data.table CJ
#' @export
plan2scenDT <- function(scenPlan
                        , type = c("ribs", "star", "sides", "crossjoin")[1:2]
                        , doAddBase = TRUE
                        , doSorted = TRUE
                        # , maxSteps = 3
){
  type <- type[1]
  if (type %in% "star"){
    ww <- lapply(seq_along(scenPlan), \(ii) as.data.table(scenPlan[ii]))
    scenDT <- rbindlist(ww, fill = TRUE)
  }

  if (type %in% c("crossjoin", "ribs", "sides")){
    # full cross join with CJ()
    scenDT <- do.call(data.table::CJ, c(scenPlan, list(sorted = doSorted)))
  }

  if (type %in% c("ribs", "sides")){
    nDim <- c(ribs = 1, sides = 2)

    scenDT <- do.call(data.table::CJ, c(scenPlan, list(sorted = doSorted)))
    for (parName in names(scenPlan)){
      scenDT[, (paste0("hasInside_", parName)) :=
               1-(get(parName) %in% range(unlist(scenPlan[[parName]])))]
    }
    scenDT[]
    InsideCols <- grep("Inside", names(scenDT), value = TRUE)
    scenDT[, scenId := seq(.N)]
    scenDT[, nDim_Inside := sum(.SD), by = scenId, .SDcols = InsideCols]
    scenDT <- scenDT[nDim_Inside <= nDim[type]]
    scenDT[, (InsideCols) := NULL]
    scenDT[, nDim_Inside := NULL]
  }
  .scenDT00 <<- copy(scenDT)

  if (doAddBase){
    scenDT <- rbind(scenDT[1], scenDT)
    scenDT[1] <- NA_real_
  }

  scenDT[, scenId := seq(.N)]
  scenDT[]
}



#' fillScenDT
#' @export
fillScenDT <- function(scenDT, pars, showStar = FALSE){
  scenDT <- copy(scenDT)
  vp <- setdiff(names(scenDT), c("scenId", "base"))
  if (is.null(pars)){
    message("pars fo fill scenDT with is NULL, just returning scenDT without taking out NA in scenDT..")
    return(scenDT) #  <- scenDT[!is.na(get(vp[1]))]
  }
  if (showStar){
    scenDT[, base := "Scenario"]
  }

  for (pp in vp){
    #TODO
    if (grepl("_add$", pp, ignore.case = TRUE)) pars[[pp]] <- 0
    if (grepl("_mult$", pp, ignore.case = TRUE)) pars[[pp]] <- 1
    if (pp %in% names(pars)){
      if (showStar){
        scenDT[is.na(get(pp)), base := "Star"]
      }
      scenDT[is.na(get(pp)), (pp) := pars[[pp]] ]
    } else {
      log_error("fillScenDT| par name {pp} not found in parameters...?")
      stop()
    }
  }
  if (showStar){
    scenDT[1, base := "Base"]
  }
  scenDT[]
}



#' makescenRanges
#' @export
makescenRanges <- function(scenDT
                           , variedParameters = setdiff(names(scenDT), "scenId")){
  # scenDT
  scenRanges <- aphMelt(scenDT)
  # huniqueN <- function(x) hlength(unique(x))
  scenRanges <- scenRanges[, hsummary(value, c("hmin", "hmax", "huniqueN", "length"))
                           , by = processName]
  scenRanges[, processName := factor(processName
                                     , levels = variedParameters
                                     , ordered = TRUE)]
  scenRanges <- scenRanges[order(processName)]
  scenRanges
}

