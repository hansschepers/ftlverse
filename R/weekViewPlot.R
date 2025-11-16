#' weekViewPlot
#' @examples \dontrun{
#'   startDataPath <- "1005/plot_clean/kbb_tuin3_1053/kbb_21_tuin_3.rds"
#'   DTcyc <- readObject(storeCustom, path = startDataPath)
#'   p <- weekViewPlot(DTcyc, wkChosen = 28:36)
#' }
#' 
#' @export
weekViewPlot <- function(DTcyc
                         , yoisoi = "growthRate"
                         , wkGrouping = 4
                         , wkChosen = NULL
                         , pggInput = list(xoi = "tow"
                                           , doplot = FALSE
                                           , xtics = 1, xlab = "Time of Week in days"
                                           # , xtics = force(1/4), xlab = "Time of day in hours"
                                           , foi = "wk", group = "wk", facet_w = "wkgroup"
                                           , fsize = 14
                                           , legend = "none", facetcols = 1, free_y = FALSE)
                         , ...
                     ){
  # (yoisCycle <- aphVariableLevels(DTcyc, direction = "long"))
    dc <- DTcyc[processName %in% yoisoi]
    dc <- dc[!is.na(value)]
    dc[, wk := lubridate::isoweek(dateTime)]
    # dc[, wk := yday(dateTime)]
    if (wkGrouping > 0){
      dc[, wkgroup := wk %% wkGrouping]
    }
    dc[, wk := format(wk, width = 2)]
    if (!is.null(wkChosen)) {
      dc <- dc[wk %in% wkChosen]
    }
    # wday(dc$dateTime)
    # dc[, tow := hour(dateTime)/24]
    dc[, tow := lubridate::wday(dateTime) -1 + lubridate::hour(dateTime)/24]
    aphKey(dc, ignoreAsDois = c("dateTime"))
    pggs(dc, input = pggInput
               , ...)
  }

# inspect dateTime per processName
if(F){
  dw <- DTcleanYois[!is.na(value)]
  dw <- dw[processName %in% yois0]
  dw <- dw[, dateTime := toMonday(dateTime)]
  dwWk <- copy(dw)[, wk := week(dateTime)]
  
  p <- pggs(dwWk, xoi = "wk", yoi = voi, foi = voi, facet_w = foic, facetcols = 1
            , geom = "point", legend = "none", doplot = FALSE
            , chunkBase = paste0(dataEnv, "_", Sys.Date(), "_")
            , chunkTitle = "available Data"
  )
  # p <- pggs(dw, yoi = voi, facet_w = foic, yearSync = 2020, facetcols = 1
  #           , geom = "point", legend = "none", doplot = FALSE)
  p
}


