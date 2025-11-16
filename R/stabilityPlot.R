#' stabilityPlot
#' @examples \dontrun{
#'   # DT is long format cycle data
#'   
#'   p <- stabilityPlot(DT)
#'   p <- stabilityPlot(DT, yoi = "value")
#' }
#' 
#' @export
stabilityPlot <- function(
  DT
  , yois = c("all", "growthRate", "GHTEMP", "RAD", "VADEFE", "OUTEMP", "ECIRRI"
              , "plantload.fruits.m2", "transpiration")
  , value.var = "value"
  # for umbrella
  , doi = c("wk", "mon", "dDate")[1]
  , time_col = intersect(names(DT), c("local_time", "dateTime"))[1]
  , foi = intersect(aphFactors(DT), c("plot_syn", "cropseason_id"))[1]
  , keep = c(".ce", ".cu", ".cu.ce", ".cu.mean", ".mean", ".zmean")[c(1, 3, 5, 6)]
  , extraUmbrellaByCols = character(0)
  # for scaling
  , scaling = c("none", "mean", "diffrange")[1]
  , subtitle = paste0("Scaling: ", scaling)
  , extraScaleByCols = character(0)
  # for plot
  , yoi = "value.ce"
  , title = "Stability Plot"
  , doplot = FALSE
  , ci.alpha = .9
  , xlab = paste("Mean performance of", doi)
  , ylab = paste("relative position of individual", foi)
  , pggsInput = list(xoi = "value.mean"
                     , xlab = xlab
                     , ylab = ylab
                     , foi = foi
                     , free_x = TRUE, mega = TRUE
                     , ci.alpha = ci.alpha, geom = "point"
                     , pointAlpha = .4
                     , labelColor = "darkblue"
                     )
  , abline = c(0, 0)
  , psize = 1.5, fsize = 8
  , ...
){
  selDT <- copy(DT)
  if (!"all" %in% yois) {
    selDT <- selDT[processName %in% yois]
  }
  if (value.var != "value"){
    setnames(selDT, value.var, "value")
  }
  
  if (isTRUE(doi[1] == "hr")  & !"hr"  %in% names(selDT)) selDT[, ':='(hr  = lubridate::hour(get(time_col)))]
  if (isTRUE(doi[1] == "doy") & !"doy" %in% names(selDT)) selDT[, ':='(doy = lubridate::yday(get(time_col)))]
  if (isTRUE(doi[1] == "wk")  & !"wk"  %in% names(selDT)) selDT[, ':='(wk  = lubridate::isoweek(get(time_col)))]
  if (isTRUE(doi[1] == "mon") & !"mon" %in% names(selDT)) selDT[, ':='(mon = lubridate::month(get(time_col)))]
  .selDT <<- copy(selDT)
  label = ifelse(isTRUE(doi == "dDate"), "none", doi)
  
  dtUmb <- aphUmbrella(selDT
                       , doi = doi
                       , foi = foi
                       , byGroups = c("processName", extraUmbrellaByCols)
                       , keep = keep)
  if (scaling[1] == "mean"){
    dtUmb[, value.ce.sc := value.ce / hmean(value.mean)
          , by = c("processName", extraScaleByCols)]
  }
  if (scaling[1] == "diffrange"){
    dtUmb[, value.ce.sc := value.ce / hdiffrange(value.mean)
          , by = c("processName", extraScaleByCols)]
  }
  .dtUmb <<- copy(dtUmb)
  pggs(dtUmb
       , input = pggsInput
       , foi = foi
       , yoi = yoi
       , title = title
       , subtitle = subtitle
       , doplot = doplot
       , abline = abline
       , psize = psize
       , fsize = fsize
       , ...
       )
}
