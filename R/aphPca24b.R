#' aphPca24b
#' 
#' @export
aphPca24b <- function(ddLong
                      , foi = "variety"
                      , phase.oi = 1:4
                      , baseTitle = ""#"Multivariate day Profile"
                      , yois = "all"
                      , anchor = c(growthRate = 2)
                      , tit = ""
                      , doplot = FALSE){
  dd_sel <- ddLong[phase %in% phase.oi]
  if (!"all" %in% yois){
    dd_sel <- dd_sel[processName %in% yois]
  }
  # aggregate
  dd_sel[, abbr := substring(variety, 8, 8)]
  dd_Agg <- dd_sel[, .(value = hmean(value))
                   , by = c(foi, "processName", "hr", "abbr")]
  dd4pca <- hdcast(dd_Agg)
  pcaArgs <- list(dd4pca
                  , foi = foi
                  , loadrad = 2
                  , loadingRadial = TRUE, lsize = 4
                  , labelCol = "hr", mega = T, pointSize = 6, labelSize = 3
                  , lineAcross = c("variety")
                  , hline = 0, vline = 0
                  , title = paste(baseTitle, tit)
                  , geom = "point"
                  , legend = "right"
                  # , more = "theme(legend.margin = margin(-5, 0, 0, 0)"
                  #      , "         , legend.box.spacing = unit(-2, 'pt'))"
                  , doplot = doplot)
  outpca <- do.call(aphPca, mergeParameters(pcaArgs))
  # outpca$rotation[names(anchor), 1:2]
  # outpca$x
  # names(attributes(outpca))
  ddd <- attr(outpca, "dfg.inclscores")
  score1 <- ddd[ddd$hr == 3 & ddd$variety == "Strabena", "PC1"]
  score2 <- ddd[ddd$hr == 3 & ddd$variety == "Strabena", "PC2"]
  load1 <- outpca$rotation[names(anchor), 1]
  load2 <- outpca$rotation[names(anchor), 2]
  mm <- list(c(-1, 1), c(1, 1), c(1, -1), c(-1, -1))[[anchor]]
  flip.ois <- c(ifelse(load1*mm[1] > 0, "", "PC1")
                , ifelse(load2*mm[2] >= 0, "", "PC2"))
  pcaArgs$flip.ois <- flip.ois
  print(flip.ois)
  outpca <- do.call(aphPca, mergeParameters(pcaArgs))
  outpca
}

if(F)
{
  out <- dt_hodAggPhase[, hsummary(value), by = processName][sumna > 100, processName]
  out <- c(out, "slabWeightLoss", "slabExhaustion"
           # , "substrate scale raw"
           , "totalPAR", "ccJ", "plant weight m2", "waterUptake", "drain")
  ddLong <- dt_hodAggPhase[!processName %in% out]
  ddLong[, abbr := substring(variety, 8, 8)]
  ppp <- lapply(1:4
                , \(phase.oi) {outpca <- aphPca24b(ddLong
                                         , phase = phase.oi
                                         , tit = paste("phase", phase.oi))
                  attr(outpca, "plot")
                }
  )
  ppp[[1]]
  Reduce(`+`, ppp)
}
