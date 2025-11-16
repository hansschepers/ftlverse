#' smoothRibbon
#'
#' @examples \dontrun{
#'   dt <- data.table(Time = 1:55, processName = "test", hmin = runif(55), hmax = runif(55))
#'   dt_sm <- smoothRibbon(dt, xoi = "Time")
#'   p1 <- pggs(dt, geom = "points") ; pggs(dt_sm, p = p1)
#' }
#' @export
# polynomial-fitted ribbons
smoothRibbon <- function(dt
                         , degree = 6
                         , xoi = c("dateTime", "local_time", "Time", "wap", "dap", "WAP", "DAP")
                         , yois = unique(dt$processName)
                         # , noSmoothYois = character(0)
                         , noSmoothYois = c("stemDensity.setting", "pruning", "LAI") # #                    , "outside_temp", "cloudiness"
                         , bycols = aphFactors(dt)#processName
){
  yois <- setdiff(yois, noSmoothYois)
  if (!length(yois)){
    return(dt)
  }
  if (degree == 0) return(dt)
  xoi <- intersect(names(dt), xoi)
  stopifnot(length(xoi) > 0)

  dtcp <- copy(dt)
  dt <- copy(dt)

  if (xoi == "dateTime"){
    formula1 = paste0("hmin ~ poly(dateTimeNum,", degree,",raw=T)")
    formula2 = paste0("hmax ~ poly(dateTimeNum,", degree,",raw=T)")
    dt[, dateTimeNum := as.numeric(difftime(dateTime
                                            , ISOdatetime(2021, 12, 1, 0,0,0)
                                            , units = "days"))]
    unSmoothed <- dt[ !processName %in% yois]
    dt <- rbindlist(list(unSmoothed
                         , dt[processName %in% yois & !is.na(hmin) & !is.na(hmax)
                              , .(dateTime = .SD[, dateTime]
                                  , dateTimeNum = .SD[, dateTimeNum]
                                  , hmin = predict(lm(eval(parse(text=formula1)), data = .SD))
                                  , hmax = predict(lm(eval(parse(text=formula2)), data = .SD))
                              ), by = bycols]
                         )
                    , fill = TRUE)
  }

  if (xoi == "local_time"){
    formula1 = paste0("hmin ~ poly(local_timeNum,", degree,",raw=T)")
    formula2 = paste0("hmax ~ poly(local_timeNum,", degree,",raw=T)")
    dt[, local_timeNum := as.numeric(difftime(local_time
                                            , ISOdatetime(2021, 12, 1, 0,0,0)
                                            , units = "days"))]
    dt <- rbindlist(list(dt[ !processName %in% yois]
                         , dt[processName %in% yois & !is.na(hmin) & !is.na(hmax)
                              , .(local_time = .SD[, local_time]
                                  , local_timeNum = .SD[, local_timeNum]
                                  , hmin = predict(lm(eval(parse(text=formula1)), data = .SD))
                                  , hmax = predict(lm(eval(parse(text=formula2)), data = .SD))
                              )
                              , by = bycols]), fill = TRUE)
  }

  if (xoi == "Time"){
    formula1 <- paste0("hmin ~ poly(", xoi, ", degree=", degree,",raw=T)")
    formula2 <- paste0("hmax ~ poly(", xoi, ", degree=", degree,",raw=T)")
    dt <- rbindlist(list(dt[ !processName %in% yois]
                , dt[processName %in% yois & !is.na(hmin) & !is.na(hmax)
                     , .(Time = Time
                         , hmin = predict(lm(eval(parse(text=formula1)), data = .SD))
                         , hmax = predict(lm(eval(parse(text=formula2)), data = .SD))
                     )
                     , by = bycols]), fill = TRUE)

  }

  if (xoi == "WAP"){
    formula1 <- paste0("hmin ~ poly(", xoi, ", degree=", degree,",raw=T)")
    formula2 <- paste0("hmax ~ poly(", xoi, ", degree=", degree,",raw=T)")
    dt <- rbindlist(list(dt[ !processName %in% yois]
                , dt[processName %in% yois & !is.na(hmin) & !is.na(hmax)
                     , .(WAP = WAP
                         , hmin = predict(lm(eval(parse(text=formula1)), data = .SD))
                         , hmax = predict(lm(eval(parse(text=formula2)), data = .SD))
                     )
                     , by = bycols]), fill = TRUE)

  }
  if ("dateTimeNum" %in% names(dt)) dt[, dateTimeNum := NULL]
  yoisAfter <- unique(dt$processName)
  lostYois <- setdiff(yois, yoisAfter)
  if (length(lostYois)){
    log_warn("you lost {length(lostYois)} variables: {paste0(lostYois, collapse = ',')}")
    dt <- rbind(dtcp[ processName %in% lostYois], dt)
  }
  dt
}

# lm(eval(parse(text="hmin ~ poly(Time, degree=6,raw=T)")), data = .dt)

# pggs(dt, geom = "ribbonlibbon", dfgRibbon = "copy")
#
