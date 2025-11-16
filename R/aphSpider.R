#' multi-variate Profile ----
#' @export
aphSpider <- function(dt.back, dt.front
                      , foi1 = "variety", foi1Chosen = "Merlice"
                      , foi2 = "grower", foi2Chosen = c("Prinsenland", "Zwinkels")
                      , labelFoi2 = TRUE
                      , yois = unique(dt.front$variable)
                      , truncLB = 0, truncUB = truncLB
                      , psize = 5
                      , violinTrim = TRUE, violinScale = "area"){
  dt.back <- copy(dt.back)
  dt.front <- copy(dt.front)
  .lastSpiderCall <<- match.call()
  
  voi <- "variable"
  nvar <- length(yois)
  truncLowers <- setNames(rep(  truncLB, nvar), yois)
  truncUppers <- setNames(rep(1-truncUB, nvar), yois)
  
  toi <- NULL
  # toi <- foi2 # "mon"
  {
    dt.backRefs <- dt.back[
      , .(ymin   = quantile(value, probs = truncLowers[get(voi)], na.rm=TRUE)
          , ymax = quantile(value, probs = truncUppers[get(voi)], na.rm=TRUE)
          , ymed = hmedian(value)
          , sdev = hsd(value))
      , by = c(voi, toi)]
    setkeyv(dt.backRefs, c(voi, toi))
  }
  
  # dt.front.b <- copy(dt.front)
  # dt.front <- copy(dt.front.b)
  # join
  dt.front <- dt.backRefs[dt.front, on = c(voi, toi)]
  dt.front[, value01 := (value - ymin)/(ymax - ymin)]
  dt.front
  print(key(dt.front))
  aphKey(dt.front)
  
  dd <- dt.front[, .(minFront = hmin(value)
                     , maxFront = hmax(value)
                     , N = length(value) - sumna(value)
                     , avgPos = hmean(value01))
                 , by = c(voi)]
  dd <- dd[dt.backRefs, on = c(voi)]
  dd[, rangeUsed := (maxFront - minFront) / (ymax - ymin)]
  .dd <<- dd
  # take out variables without foreGround data
  varOut <- union(dd[apply(dd, 1, sumna) > 0, variable], dd[N <= 2, variable])
  dt.front <- dt.front[!get(voi) %in% varOut]
  dt.backRefs <- dt.backRefs[!get(voi) %in% varOut]
  .dt.front <<- dt.front
  p.violin <- pggs(dt.front
                   # , p = p.frontPointLine
                   , xoi = voi, yoi = "value01"
                   , foi = voi, facet_w = "nothing"
                   , legendTitle = foi2, legend = "none"
                   , fsize = 16, xlab = NULL, ylab = NULL, xangle = 30
                   , ysc = c(0, 1), ytics = 1, geom = "violin", psize = 9, addgrad = TRUE
                   , violinTrim = violinTrim
                   , violinProbs = NULL
                   , violinScale = violinScale
                   # , label = foi2, mega = TRUE
                   , hline = c(0,1), ablinecolor = "grey"
                   , noLegendItem = voi
                   , doplot = FALSE, allowppt = FALSE
                   , title = paste0(foi1, " violin: ", foi1Chosen)
  )
  
  if (length(foi2Chosen)){
    dt.superfront <- dt.front[get(foi2) %in% foi2Chosen]
    .dt.superfront <<- dt.superfront
    
    if (labelFoi2){
      foi2label <- foi2
      foi2mega <- TRUE
    } else {
      foi2label = "none"
      foi2mega <- FALSE
    }
    p.front <- pggs(dt.superfront
                    , p = p.violin
                    , xoi = voi, yoi = "value01"
                    , foi = foi2, legendTitle = foi2
                    , facet_w = "nothing", psize = psize
                    , fsize = 16, xlab = NULL, ylab = NULL, xangle = 30
                    , ysc = c(0, 1), ytics = 2, yticsShift = -.2
                    , geom = "pointline"#, addgrad = TRUE
                    , label = foi2label, mega = foi2mega
                    , hline = c(0,1), ablinecolor = "grey"
                    , doplot = FALSE, allowppt = FALSE
                    , noLegendItem = voi
                    , title = paste0(foi1, " profile: ", foi1Chosen)
    )
  } else {
    p.front <- p.violin
  }
  p.front
  {
    # add limit texts ----
    dt.backRefs[, labelMin := hprettyNum(ymin, 1)]
    dt.backRefs[, labelMax := hprettyNum(ymax, 1)]
    p.front <- addText(p.front, dt.backRefs, xoi = voi, yoi = 0, laboi = "labelMin", size = 4, vjust = 1.1)
    p.front <- addText(p.front, dt.backRefs, xoi = voi, yoi = 1, laboi = "labelMax", size = 4, vjust = -0.1)
    p.front
  }
  # vrmd("add", p = p.front, chunkId = paste0(foi1, "profile: ", foi1Chosen))
  .p.front <<- p.front
  p.front
}



# log_threshold(INFO)
# if(!is.null(toi)){
#   p.ribbon <- pggs(dt.backRefs, xoi = foi2, yoi = "ymed", geom = "pointline", xtics = 1
#                    , fsize = 12
#                    , ribbon = TRUE, ribbonColor = "darkgreen", ribbonAlpha = .3
#                    , noLegendItem = voi
#                    , chunkTitle = "monthly ribbons"
#   )
# }

