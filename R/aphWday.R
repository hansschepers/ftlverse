#' aphWeek
#' @examples \dontrun{
#'   testDate <- ymd("2011-3-24")
#'   testDate <- ymd("2011-12-30")
#'   testDate <- ymd("2012-01-01")
#'   data.table::week(testDate + years(1:11))
#'   data.table::isoweek(testDate + years(1:11))
#'   lubridate::isoweek(testDate + years(1:11))
#'   lubridate::week(testDate + years(1:11))
#'   aphWeek(testDate + years(1:11), xm52 = FALSE)
#'   aphWeek(testDate + years(1:11), xm52 = TRUE)
#' }
#' @export
aphWeek <- function(x, week_start = 1, xm52 = FALSE, ...) {
  res <- lubridate:::.other_week(x = x, week_start = week_start)
  if (xm52){
    res[lubridate::month(x) == 12 & res %in% c(1, 53)] <- 52
    res[lubridate::month(x) == 1  & res %in% c(52, 53)] <- 1
  }
  res
}

#' aphWday
#' @export
aphWday <- function(x
                    , label = FALSE
                    , abbr = TRUE
                    , week_start = getOption("lubridate.week.start", aphWeekstart)
                    , locale
                    , lang.oi = "en"
                    , aphWeekstart = 1
){
  if (lang.oi == "local"){
    locale <- Sys.getlocale("LC_TIME")
  } else {
    locale <- paste0(tolower(lang.oi), "-", toupper(lang.oi))
  }
  # getOption("lubridate.week.start")
  lubridate::wday(x
                  , label = label
                  , abbr = abbr
                  , week_start = week_start 
                  , locale = locale)
}

#' aphWeekday
#' @export
aphWeekday <- function(x
                       , label = TRUE
                       , abbr = TRUE
                       , ...){
  aphWday(x, label = label, abbr = abbr, ...)
}


if(F){
  aw <- 1
  dd <- data.table(dDate = c(ymd("2020-01-01") + 0:8
                             , ymd("2020-12-22") + 0:22
                             , ymd("2017-12-30") + years(1:22) ))
  dd[, lubri_week := lubridate::week(dDate)]
  dd[, dt_week := data.table::week(dDate)]
  dd[, weekno0 := as.numeric(format(dDate, "%W"))]
  dd[, weekno := as.numeric(format(dDate, "%W"))]
  {
    dd[, year := as.numeric(format(dDate, "%Y"))]
    dd[, weekno_max := max(weekno), by = "year"]
    # shift weeknos by 1 if maxweek is 53
    dd[weekno_max == 53, weekno := weekno - 1, by = "year"]
    # shift year by 1 if weekno is 0
    dd[weekno == 0, year := year - 1, by = weekno]
    # recalculate because the week is in a different year now
    dd[, weekno_max := max(weekno), by = "year"]
    dd[weekno == 0, weekno := weekno_max, by = weekno]
    # cleanup
    dd[, weekno_max := NULL]
  }
  dd[, proc_week := as.numeric(format(dDate, "%W"))]
  dd[, isoweek := lubridate::isoweek(dDate)]
  dd[, aph_week := aphWeek(dDate, week_start = aw)]
  dd[, aph_weekOK := aphWeek(dDate, week_start = aw, xm52 = FALSE)]
  dd[, aphwday := aphWday(dDate, week_start = aw)]
  dd[, aphweekday := aphWeekday(dDate, abbr = FALSE, lang.oi = "nl")]
  print(as.data.frame(dd))
}


if(F){
  dd2 <- data.table(dDate = ymd("2022-09-01") + 0:(100*365))
  dd2[, aph_week := aphWeek(dDate, week_start = aw)]
  dd2[, aph_weekOK := aphWeek(dDate, week_start = aw, xm52 = FALSE)]
  plot(dd2$dDate, dd2$aph_week)
  plot(dd2$dDate, dd2$aph_weekOK)
  (tt <- table(dd2$aph_week))
  plot(tt, type = "p")
  (ttOK <- table(dd2$aph_weekOK))
  plot(ttOK, type = "p")
  dd2[aph_weekOK == 53]
}
