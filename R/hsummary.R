#' hsummary
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hsummary <- function(x, hfuns = c("quick", "stream", "ribbon", "mini", "mimanna"
                                  , "refstats1", "refstats2", "tails", "full")[1]
                     , extraFuns = character(0)
                     , ...) {
  x <- unlist(x)
  dots <- list(...)
  if (is.character(hfuns)){
    if (!length(hfuns)) { return(list()) }
    if (hfuns[1] == "quick") {
      hfuns <- c("hmean", "hmin", "hmax", "hN", "sumna", "hI")
    }
    if (hfuns[1] == "dateInfo") {
      hfuns <- c("hmin", "hwdayMean", "hwdayDiffrange", "percentMonday", "hmax")
    }
    if (hfuns[1] == "stream") {
      hfuns <- c("hmean", "hmin", "hmax", "first", "last", "hdiffrange")
    }
    if (hfuns[1] == "ribbon") {
      hfuns <- c("hmean", "hmin", "hmax")
    }
    if (hfuns[1] == "traj") {
      hfuns <- c("hmean", "hmin", "hmax", "hsd", "hfirst", "hlast")
    }
    if (hfuns[1] == "tails") {
      hfuns <- c("leftNAcount", "rightNAcount", "leftZerocount", "rightZerocount", "notSuspect")
    }
    if (hfuns[1] == "mimanna") {
      hfuns <- c("hmin", "hmax", "hlength", "sumna")
    }
    if (hfuns[1] == "mini") {
      hfuns <- c("hlength", "sumna", "okData")
    }
    if (hfuns[1] == "refstats1") {
      hfuns <- c("hmean", "hmin", "hmax", "hlength", "sumna", "hIQR"
                 , "hquantile01", "hquantile05", "hquantile95", "hquantile99")
    }
    if (hfuns[1] == "refstats2") {
      hfuns <- c("hmedian", "hIQR", "hquantile05", "hquantile95")
    }
    if (hfuns[1] == "full") {
      hfuns <- c("hmedian", "hmean", "hsd", "hmin", "hmax", "hIQR", "hmedian"
                 , "hquantile01", "hquantile05", "hquantile25", "hquantile75", "hquantile95", "hquantile99"
                 , "hskewness", "hkurtosis"
                 # , "htsd", "hmad"
                 , "hmadmean", "hlb", "hub", "hcutLow", "hcutHigh"
                 # , "hIQR"
                 , "hlength"
                 # , "hsum"
                 , "hdiffrange"
                 , "sumna"
                 , "okData"
                 , "sum0")
    }
    # c(hfuns, extraFuns)
    resList <- lapply(c(hfuns, extraFuns)
                      , function(f) do.call(f, c(list(x=x), dots)))
    resList <- setNames(resList, c(hfuns, extraFuns))
  } else {
    hfuns <- as.list(c(hfuns))
    resList <- lapply(hfuns
                      , function(f) do.call(f, c(list(x=x), dots)))
  }
  return(resList)
}


#' hsummaryC
#' @author Hans Schepers, \email{hans.schepers@@bayer.com}
#' @export
hsummaryC <- function(x, digits=2, ...) {
  xx <- hsummary(as.numeric(unlist(x)), ...)
  setNames(lapply(xx, function(x) hprettyNum(x , digits=digits))
           , names(xx))
}
