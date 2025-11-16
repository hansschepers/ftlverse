#' simsSummary
#' @examples \dontrun{
#'   simsSummary(SIMS)
#'   simsSummary(list(a = SIMS, b = SIMS))
#'   simsSummary(SIMS$cropLong, extraFuns = c("hsum", "hlength"))
#'   simsSummary(SIMS$cropLong, aggregate.fun = "hmean")
#'   simsSummary(SIMS$cropLong, focus = "pb_table")
#'   simsSummary(SIMS$cropLong, aggregate.fun = character())
#'   simsSummary(SIMS$cropLong, aggregate.fun = "hmean", extraFuns = character())
#'   simsSummary(SIMS$cropLong, digits = -2)
#' }
#' @export
simsSummary <- function(SIMS
                        , costFunction = data.table()
                        , focus = c("commercialKPIs")
                        , yois = aphKpis(focus)
                        , extraYois = c("surplus")#character()
                        , omit = c("surplus", "settingSuccess")
                        , yoisSum = c("yield", "harvested.fruits"
                                      , "trussSpeed", "growthRate"
                                      , "waterSupply")
                        , filterExpression = TRUE  # e.g. filter Time
                        , doPretty = TRUE
                        , doFull = FALSE
                        , digits = 3
                        , aggregate.fun = "ribbon"
                        , extraFuns = "hsum"
                        , bycols = character()
                        , variable.name = "variable"
                        , addTiming = FALSE
                        , ...
){
  if (inherits(SIMS, "data.table")){
    # SIMS is a dt cropLong
    SIMS <- list(cropLong = SIMS)
  }

  #    ==================================================== RECURSIVE CALL =====
  if (inherits(SIMS[[1]], "list")){
    log_debug("SIMS is a scenList")
    kpisList <- lapply(SIMS, simsSummary
                       , costFunction = costFunction
                       , focus = focus
                       , yois = yois
                       , extraYois = extraYois
                       , omit = omit
                       , yoisSum = yoisSum
                       , filterExpression = filterExpression
                       , doPretty = doPretty
                       , doFull = doFull
                       , digits = digits
                       , aggregate.fun = aggregate.fun
                       , extraFuns = extraFuns
                       , bycols = bycols
                       , variable.name = variable.name
                       , addTiming = addTiming
                       , )
    df_kpi <- rbindlist(kpisList, idcol = "scenId")
    return(df_kpi)
  }

  #    ==================================================== START ==============
  tmp <- copy(SIMS$cropLong)
  daysPerStep <- SIMS$daysPerStep
  daysPerWeek <- 7

  yois <- union(yois, extraYois)
  yois <- setdiff(yois, omit)

  if (is.null(daysPerStep)) {
    log_warn("simsSummary| assumed: daysPerStep = 7")
    {
      str(sys.calls())
      print(sys.frames())
      print(sys.parents())
      print(sys.frame(-1)); print(parent.frame())
      # stop()
    }
    daysPerStep <- 7
  }

  if (nrow(costFunction) > 0){
    message("using costFunction")
    costFunction <- copy(costFunction)
    yoisSIMS <- unique(tolower(SIMS$cropLong$processName))
    stopifnot(all(tolower(costFunction$processName) %in% yoisSIMS))
    costFunction[, kpi := 0]
    costFunction[, score := 0]
    costFunction[, index := seq(.N)]
    costFunction[, crit := paste(c(index, processName, aggregation)
                                 , collapse = "__"), by = index]
    print(costFunction)
    message("55")
    ii <- 1
    for (ii in seq(nrow(costFunction))){
      tmp <- copy(SIMS$cropLong)
      message("cf line----", costFunction[ii, crit])
      cF <- as.list(costFunction[ii])
      tmp <- tmp[tolower(processName) %in% tolower(cF$processName) &
                   Time >= (7*cF$week_min) &
                   Time <= (7*cF$week_max)]
      # str(tmp)
      if (tolower(cF$aggregation) == "mae"){
        costFunction[ii, kpi := MAE(truth = cF$thresholdOrTarget, estimate = tmp$value)]
      } else {
        aggFUN <- get(cF$aggregation)
        stopifnot(names(formals(aggFUN))[1] == "x")
        costFunction[ii, kpi := do.call(aggFUN, list(x = tmp$value))]
      }
      costFunction[ii, score := weight * kpi]
    }
    costFunction[, scoreSum := hsum(score)]
    return(costFunction)
  }

  {
    # d1 <- dim(tmp)
    tmp <- tmp[eval(filterExpression)]
    # d2 <- dim(tmp)
    # message("difference in cropLong size")
    # print(d2 - d1)
    dtSummary <- tmp[processName %in% yois
                     , hsummary(value
                                , hfuns = aggregate.fun, extraFuns = extraFuns)
                     , by = c("processName", bycols)]
    if ("hsum" %in% c(aggregate.fun, extraFuns)){
      dtSummary[, hsum := hsum * daysPerStep/daysPerWeek]
    }
    # compareNames(.dtSummary$processName, yois)
    # .dtSummary <<- copy(dtSummary)
  }

  suppressWarnings({
    dtSummary[!processName %in% yoisSum
              , hsum := NA]
  })

  dtSummary[, processName := fixFactor(dtSummary$processName
                                       , xlevels = yois #aphKpis("pb_table")
                                       , keepAll = TRUE)]
  aphKey(dtSummary)
  # .dtSummarySorted <<- copy(.dtSummary)
  dtSummary

  if (doPretty){
    dtSummary <- hprettyNum(dtSummary
                            , digits = abs(digits)
                            , asNum = (digits > 0))
    # isTRUE(all( is needed: all can handle length != 1, isTRUE can assess NA
    # alternative would be all(..., na.rm = TRUE)
    if (isTRUE(all(aggregate.fun == "ribbon"
                   , "hsum" %in% extraFuns
                   , length(c(aggregate.fun, extraFuns)) >= 2))){
      suppressWarnings({
        dtSummary[!processName %in% yoisSum
                  , hsum := ""]
      })
      if (variable.name != "processName"){
        if ("processName" %in% names(dtSummary)) setnames(dtSummary, "processName", variable.name)
      }
      if ("hmean" %in% names(dtSummary)) setnames(dtSummary, "hmean", "Average")
      if ("hmin" %in% names(dtSummary)) setnames(dtSummary, "hmin", "Minimum")
      if ("hmax" %in% names(dtSummary)) setnames(dtSummary, "hmax", "Maximum")
      if ("hsum" %in% names(dtSummary)) setnames(dtSummary, "hsum", "Sum")
      if ("hlength" %in% names(dtSummary)) setnames(dtSummary, "hlength", "N")
      # setnames(dtSummary, c("variable" , "Average", "Minimum", "Maximum", "Sum"))
    }

    if(addTiming & !is.null(SIMS$daysPerStep)){
      YY <- dtSummary[get(variable.name) == "yield", Sum]
      dt_early <- earlinessKpis(SIMS, yieldMilestones = c(1, YY/2, YY))$WAP
      lastLine <- data.table(variable = "harvest_timing"
                             , Average = dt_early[2]
                             , Minimum = dt_early[1]
                             , Maximum = dt_early[3]
                             , Sum = dt_early[3] - dt_early[1])
      setnames(lastLine, "variable", variable.name)
      dtSummary <- rbind(dtSummary, lastLine)
    }

    return(dtSummary[])
  }

  # .dtSummary1 <<- copy(dtSummary)
  # dtSummary <- copy(.dtSummary1)
  dd <- aphMelt(dtSummary)
  if ("df_delta" %in% names(SIMS)){
    if (ncol(SIMS$df_delta) > 0){
      dfd <- copy(SIMS$df_delta)
      dfd[, sim := NULL]
      dfd[, expected := NULL]
      stopifnot(all.equal(names(dd), names(dfd)))
      dd <- rbindlist(list(dd, SIMS$df_delta), fill = TRUE)
    }
  }
  dd[, pn := paste(kpi, processName, sep = "_")][]
  if (doFull) return(dd)

  dd[, processName := NULL]
  dd[, kpi := NULL]
  dd
  # .dd <<- copy(dd)
  dcast(dd, . ~ pn)
  # hdcast(dd, rhs = "pn")

  # find first missSetting
  # missSetTimes <- tmp[processName %in% c("settingSuccess")
  #                     , .(Time = Time
  #                         , value = hcumsum(value) / hcumsum(!is.na(value)))
  # ][value < settingSuccessThreshold, Time]
  # df_kpiFIRST <- data.table(processName = "firstMissSetting"
  #                           , value = ifelse(length(missSetTimes) >= 1
  #                                            , missSetTimes[1], 400))
}
