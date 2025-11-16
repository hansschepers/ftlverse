#' ribbon
#' @examples \dontrun{
#'   ribbon(DT = dtmr2
#'   , yois = c("rh", "absHum", "temperature")
#'   , ribbonColor = "purple"
#'   #, ribbonAlpha = 1
#'   , xsc = c(170, 230), geom = "ribbonlibbon")
#' }
#' @export
ribbon <- function(DT
                   , yois = NULL
                   , variable.name = "variable"
                   , xoi = "doy"
                   , by = unique(c(variable.name, xoi))
                   , hfuns = c("hmean", "hmin", "hmax")
                   , geom = ""
                   , ...){
  if (!grepl("ibbon", geom)){
    geom <- paste0(geom, "ribbon")
  }
  if (length(yois)){
    dd <- DT[get(variable.name) %in% yois]
  } else {
    dd <- copy(DT)
  }
  dd <- dd[, hsummary(value, hfuns = hfuns), by = by]
  .dd <<- dd
  d1 <- copy(dd)
  d1[, (xoi) := get(xoi) - hours(12)]
  d2 <- copy(dd)
  d2[, (xoi) := get(xoi) + hours(11)]
  dd12 <- rbind(d1, d2)
  # setnames(dd12, "xoi", xoi)
  setkeyv(dd12, by)
  .dd12 <<- dd12
  # print(head(dd, 2))
  pggs(dd12
       , geom = geom, ymin = "hmin", ymax = "hmax", ...)
}
