#' starPlot
#' @examples \dontrun{
#'   require(ggplot2)
#'   loads <- data.frame(PC1 = c(-2, 2, 3, 3), PC2 = c(1, 4, 4, 1), variable = paste("Variable", 1:4))
#'   loads2 <- data.frame(PC1 = .3+c(-2, 2, 3, 3), PC2 = c(1, 4, 4, 1), variable = paste("Variable", 1:4))
#'   loads = rbindlist(list(foi1 = loads, foi2 = loads2), idcol = "foi")
#'   starPlot(loads)
#'   starPlot(loads, pole = c(2.5, 1.5))
#'   starPlot(loads, pole = c(2.5, 1.5), foi = "foi")
#'   loads_agg <- hdcast(aphAggregate(aphMelt(loads), accross = "variable"))
#'   loads_joined <- loads[loads_agg, on = "foi"]
#'   starPlot(loads_joined, foi = "foi", poleXYnames = paste0("i.", c("PC1", "PC2")))
#'   #
#'   p <- starPlot(loads, loadingRadial = TRUE) # only for pole = c(0,0)
#'   p$layers[[1]]$data # for arrows
#'   p$layers[[2]]$data # for labels
#' }
#' @rawNamespace import(ggplot2, except = last_plot)
#' @export
starPlot <- function(DT
                     , pole = c(0, 0)
                     , poleXYnames = NULL
                     , foi = character(0)
                     , p = ggplot()
                     , xy = paste0("PC", 1:2)
                     , relloadrad = 0
                     , arrowList = arrow(length = unit(0.15, "inches"))
                     , loadwidth = 1
                     , loadcolor = "black"
                     , geomLoad = "arrowlabel"
                     , labelCol = "variable" #character(0)
                     , labelSize = 3
                     , loadLabelColor = "red"
                     , loadingRadial = FALSE
                     , space = 0.15
                     , outward.loadings = TRUE
                     , just.random = 0
                     , pal.oi = "pal.std"
                     , addgrad = FALSE
){
  loads <- as.data.frame(DT)[, union(xy, labelCol)]
  if (is.null(poleXYnames)){
    center <- loads
    center[, 1] <- pole[1]
    center[, 2] <- pole[2]
    poleXYnames <- xy
  } else {
    center <- as.data.frame(DT)[, poleXYnames]
  }
  # .center <<- center
  just.out <- ifelse(outward.loadings, 1, -1)
  
  
  if (grepl("arrow", geomLoad)) {
    dfg.loadingArrows <- cbind(center[, poleXYnames] + (loads[, xy] - center[, poleXYnames]) * relloadrad
                               , loads[, xy])
    names(dfg.loadingArrows) <- c("x", "y", "xe", "ye")
    dfg.loadingArrows$arrowColor <- "black"
    if (length(foi)){
      foi <- foi[1]
      xx <- as.data.frame(DT)[, foi]
      .xx <<- xx
      dfg.loadingArrows$arrowColor <- as.character(xx)
    }
    .dfg.loadingArrows <<- dfg.loadingArrows
    p <- p + geom_segment(data = dfg.loadingArrows
                          , aes(x = x,y = y, xend = xe, yend = ye, color = arrowColor)
                          , arrow = arrowList
                          , size = loadwidth) #+ 
      # facet_wrap(~arrowColor, drop = TRUE )
  }
  
  if (grepl("label", geomLoad)) {
    if (!labelCol %in% names(loads)){
      message("no variable column")
    }
    loads$angles <- 0
    if (loadingRadial) {
      loads$angles <- 180 * atan2(loads[, xy[2]], loads[, xy[1]])/pi
      loads[loads$angles > 90, "angles"] <- loads[loads$angles > 90, "angles"] - 180
      loads[loads$angles < -90, "angles"] <- loads[loads$angles < -90, "angles"] + 180
      loads$vv <- 0.5
    } else {
      loads$vv <- -space + (1 + 2 * space) * (just.out * loads[, xy[2]] < 0)
    }
    loads$hh <- -space + (1 + 2 * space) * (just.out * loads[, xy[1]] < 0)
    loads$hh <- loads$hh + (runif(nrow(loads)) - 0.5) * just.random
    loads$vv <- loads$vv + (runif(nrow(loads)) - 0.5) * just.random
    p <- p + geom_text(data = loads
                       , aes_string(x = xy[1], 
                                    y = xy[2], angle = "angles", label = labelCol, 
                                    hjust = "hh", vjust = "vv")
                       , size = labelSize
                       , color = loadLabelColor)
  }
  
  pal.std <- c("black", "red", "green", "blue"
               , "magenta", "orange", "cyan", "violet"
               , "darkred", "darkgreen", "darkblue", "gray")
  # if (pal.oi != "pal.std"){
  # message(pal.oi)
  # cp <- ggthemes::canva_palettes
  # if (pal.oi[1] %in% names(cp)) {
  #   palette.oi <- cp[[pal.oi]]
  # } else {
  #   if (exists(base::get(pal.oi), mode="character")) {
  #     stop("palette not found")
  #   } else {
      palette.oi <- base::get(pal.oi)
  #   }
  # }
  palette.oi = rep(palette.oi,100)
  p = p + scale_fill_manual( values=palette.oi)
  p = p + scale_colour_manual( values=palette.oi)
  
  
  .loads <<- loads
  return(p)
}

