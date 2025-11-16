# wrangle & viz -------------------------------------------------------
#' plotRibbons
#' @export
plotRibbons <- function(scenList
                        , pggsInputRibbon = list(legend = "none"
                                                  , geom = "libbonribbon"
                                                  , libbonSize = 1
                                                  , libbonAlpha = .3
                                                  , libbonLineType = 1
                                                  , libbonMinColor = "grey"
                                                  , libbonMaxColor = "grey"
                                                  , ribbonAlpha = .2
                                                  , ribbonColor = "yellow")
                        , pggsInput = list(geom = "line"
                                           , foi = "scenId"
                                           , lwd = 1, lineAlpha = .2)
                        , yois 
                        , showScenarios = FALSE
                        , showHighlight = numeric(0)
                        , traTable = c("none", "sim2data")[1]
                        , dfgRibbon = "auto"
                        , ...
){
  pggsInputRibbon <- mergeParameters(pggsInputRibbon, list(...))
  pggsInput <- mergeParameters(pggsInput, list(...))
  
  ddL <- lapply(scenList, \(x) trapro(x$cropLong, traTable))
  d_p <- rbindlist(ddL, idcol = "scenId")
  
  p_rib <- pggs(d_p[processName %in% yois]
                , input = pggsInputRibbon
                , dfgRibbon = dfgRibbon)
  if (showScenarios){
    p_rib <- pggs(d_p[processName %in% yois]
                 , input = pggsInput
                 , p = p_rib)
  }
  
  if (length(showHighlight)){
    p_rib <- pggs(d_p[processName %in% yois & scenId %in% showHighlight]
                  , input = pggsInput
                  , p = p_rib
                  , lwd = 1.5, lineAlpha = 1
                  , lineColor = "blue")
  }
  p_rib
}


# d_p <- d_p[scenId %in% ok]
# x <- scenList[[1]]
# test <- trapro(x$cropLong, "sim2data")
# unique(test$processName)
# test[processName %in% yois][, unique(processName)]
