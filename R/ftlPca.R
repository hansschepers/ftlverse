#' aphPca
#' 
#' @rawNamespace import(ggplot2, except = last_plot)
#' @export
aphPca <- function (dfg
                    , yois = aphVariableLevels(dfg)
                    , subtractTimevarFromYois = TRUE
                    , foi = aphFactors(dfg)
                    , labelCol = NULL
                    , labelRepel = 0
                    , color = NULL
                    , group = union(foi, c(color, labelCol))
                    , doScale = 1
                    , do_cleanRows = TRUE
                    , do_cleanSD0 = TRUE
                    , data.oi = "base" 
                    , lang.oi = "none"
                    # , lang.oi = "ee"
                    , tl3Key = "en"
                    , cloudKB = FALSE, kb.local = NULL
                    , loading.oi = "fresh", saveLoading = "none"
                    , factoraxes = c(1, 2)
                    , plotaxes = 0  # is linewidth! 0 = off/false
                    , doPCAonly = FALSE, addScores = TRUE, rel.min.dist = 0, loadwidth = 1
                    , dict = NULL
                    , geomLoad = "arrowlabel"
                    , showSuffix = TRUE
                    , loadingRadial = FALSE, lsize = 6, space = 0.15
                    , loadLabelMod = function(x, ...) {x}
                    , loadcolor = "gray", loadLabelColor = "black"
                    , loadLabelFun = c(ggplot2::geom_text
                                       , ggrepel::geom_text_repel
                                       , ggrepel::geom_label_repel)[[1]]
                    , showLoadLabelModulo = 1
                    , loadrad = 4, relloadrad = 0, outward.loadings = TRUE
                    , arrowList = list(angle = 30, length = unit(0.15, "inches"), ends = "last", type = "open")
                    , yoisaxis = "" #character(0)
                    , naxis = min(3, max(0, length(yoisaxis)))
                    , yoisaxistype = c("none", "auto", "range", "given")[3]
                    , axisdigitsgiven = rep(2, naxis), yoiStepgiven = rep(1, naxis)
                    , orangeoutside = FALSE
                    , outward.scores = TRUE, labelSize = 5, mega = FALSE, geom = "point"
                    , lwd = 1.2, fsize = 14, psize = 3, pointAlpha = 0.4
                    , chunkTitle = ""
                    , title = chunkTitle
                    # , smoothFactor = "none"
                    , timevar = "week"
                    , lineAcross = timevar
                    , xextra = c(0.2, 0.2)
                    , xlab = "auto", ylab = "auto", flip.ois = NULL
                    , use = c("pairwise.complete.obs", "complete")[1]
                    , just.random = 0
                    , doDebug = FALSE
                    , doplot = TRUE
                    , input = list()
                    , pfx = "zxzx"
                    , ...) 
{
  
  
  # parse input if present #####################################################
  if (exists("g.aphPcaInput") & is.null(input)){
    log_debug("using global object 'g.aphPcaInput'. .")
    input <- g.aphPcaInput
  }
  
  if (length(input)){
    funFormals <- formals(aphPca)
    if (!"list" == class(input)[1]) {
      input <- shiny::isolate(shiny::reactiveValuesToList( input ))
    }
    names(input) <- gsub(pfx, "", names(input))
    # ignored <- base::setdiff(names(input), names(funFormals))
    common <- intersect(names(input), names(funFormals))
    input <- input[common]
    if (doDebug) str(input)
    
    CALL <- match.call()
    for (nm in names(input)) {
      if (!nm %in% names(CALL)) {  # don't use if specified in Call
        log_debug("using from input: {nm}, value {input[[nm]]}")
        assign(nm, input[[nm]])
        if (nm %in% names(funFormals)){
          if (class(funFormals[[nm]]) == "numeric") {
            log_debug("making numeric: {nm}")
            str(nm)
            assign(nm, as.numeric(get(nm)))
          }
        }
      }
    }
  }
  .CALLpca <<- match.call()
  
  ###########################################################
  
  # if (doDebug) {
  #   rmDotObjects()
  # }
  if ("value" %in% yois){
    log_error("you likely need to cast to wide format?")
    stop("e.g. type 'dfg <- hdcast(as.data.table(dfg))'")
  }
  dfg <- as.data.frame(dfg)
  if (!length(foi)) {
    dfg[, "r"] <- hseq(nrow(dfg))
    foi <- "r"
  }
  m <- as.data.frame(dfg)
  if (!is.null(foi)) {
    if (length(foi) > 1) {
      log_debug("fois = {foi}")
      m <- haddKey(m, fois = foi)
      foi <- paste(foi, collapse = "_")
    }
    log_debug("foi = {foi}")
    m[, foi] <- as.factor(m[, foi])
  }
  if (!is.null(color)) {
    if (!color %in% names(m)) {
      color <- foi
    }
  } else {
    color <- foi
  }
  str(group)
  if (length(group)) {
    if (length(group) > 1) {
      log_debug("group = {group}")
      m <- haddKey(m, fois = group)
      group <- paste(group, collapse = "_")
    }
    log_debug("group = {group}")
    if (!group %in% names(m)) {
      group <- foi
    }
  } else {
    group <- foi
  }
  if (length(labelCol)) {
    if (!labelCol %in% names(m)) {
      log_error("columns {labelCol} not found")
      # labelCol <- foi
    }
  }
  if (length(labelCol)) {
    if (labelCol == "none") labelCol <- NULL
  }
  
  numericCols <- aphVariableLevels(dfg, direction = "wide")
  yois <- intersect(yois, numericCols)
  yois <- setdiff(yois, c(labelCol, foi, group))
  
  if (subtractTimevarFromYois) {
    yois <- setdiff(yois, timevar)
  }
  
  yoisaxis <- intersect(yoisaxis, numericCols)
  dfg <- m
  
  m <- as.data.frame(m)
  cat("starting dimension: ")
  print(dim(m))
  m.c <- m
  m <- m.c
  if(do_cleanRows){
    m <- clean2(m, yois)
    rowouts <- nrow(m.c) - nrow(m)
    if (rowouts) {
      message(rowouts, "rows taken out with NA")
    }
  }
  
  if(do_cleanSD0){
    m.c2 <- m
    .mBeforeCleaningColumns <<- m
    m <- cleanSD0(m, yois)
    
    colouts <- ncol(m.c2) - ncol(m)
    if (colouts) {
      message(colouts, " columns taken out with SD=0")
      takenout <- setdiff(names(m.c2), names(m))
      message(paste(takenout, collapse = " | "))
    }
  }
  m2 <- m
  # hshead(m2)
  .yois <<- yois
  yois <- intersect(yois, names(m2))
  if (doDebug) {
    .yois2 <<- yois
    .m2 <<- m
  }
  m <- m[, yois]
  if (prod(dim(m)) == 0) {
    log_fatal("no data left: {dim(m)}")
    return(NULL)
  }
  
  if (doScale == 1) {
    ms <- scale(m)
  } else {
    ms <- m
  }
  correlmatrix <- cor(ms, use = use)
  if (doDebug){
    .correlmatrix <<- correlmatrix
  }
  ########################################################## corr made #########
  
  
  if (cloudKB & is.null(kb.local)) {
    if (exists("kb.li")){
      log_info("reading kb.li")
      kb.local <- kb.li
    }
  }
  if (!data.oi %in% names(kb.local)) {
    kb.local[[data.oi]] <- list()
  }
  
  ######################################################### fresh prcomp() #####
  if (tolower(loading.oi) == "fresh") {
    .ms <<- ms
    outpca <- prcomp(ms
                     , retx = TRUE
                     , center = FALSE
                     , scale = FALSE
    )
    .outpca <<- outpca
    # score = TRUE, cor = TRUE, 
    for (flip.oi in flip.ois) {
      if (flip.oi %in% dimnames(outpca$rotation)[[2]]) {
        outpca$rotation[, flip.oi] <- -outpca$rotation[, 
                                                       flip.oi]
      }
    }
    kb.local[[data.oi]][["outpca"]] <- outpca
    
  } else {
    
    #################################################### existing rotation #####
    
    # if (tolower(loading.oi) == "last") {
    #   cat("taking global last loading from kb.local")
    #   if (!"outpca" %in% names(kb.local[[data.oi]])) 
    #     return("no last outpca found in kb")
    #   outpca <- kb.local[[data.oi]][["outpca"]]
    # }
    # else {
    log_info("reading outpca from kb.local[[{loading.oi}]]")
    if (!loading.oi %in% names(kb.local)) {
      return(paste0("no slot ", loading.oi, " found in kb"))
    }
    if (!"outpca" %in% names(kb.local[[loading.oi]])) {
      return("no outpca found in kb")
    }
    outpca <- kb.local[[loading.oi]][["outpca"]]
  }
  
  if (saveLoading != "none") {
    if (!saveLoading %in% names(kb.local)) {
      kb.local[[saveLoading]] <- list()
    }
    kb.local[[saveLoading]][["outpca"]] <- outpca
  }
  
  if (cloudKB) {
    if (!"KNOW" %in% search()) {
      message("creating & attaching environment 'KNOW'")
      KNOW <- new.env()
      attach(KNOW, name = "KNOW")
    }
    assign("kb.li", kb.local, pos = "KNOW")
  }
  
  if (doDebug) {
    .kb.local <<- kb.local
  }
  
  ww <- outpca$sdev
  expl <- ww^2/sum(ww^2)
  if (xlab == "auto") {
    xlab <- pcaAxisLabels(expl, factoraxes[1])
    # xlab <- paste0("PC", factoraxes[1], " (", round(100 * 
    #                                                   expl[factoraxes[1]], 0), "%)")
  }
  if (ylab == "auto") {
    ylab <- pcaAxisLabels(expl, factoraxes[2])
    # ylab <- paste0("PC", factoraxes[2], " (", round(100 * 
    #                                                   expl[factoraxes[2]], 0), "%)")
  }
  print(paste(xlab, ylab))
  # print(foi)
  
  loading <- outpca$rotation
  scores <- as.data.frame(as.matrix(ms) %*% loading)
  
  if (doPCAonly) addScores <- TRUE
  if (addScores) {
    loadings <- outpca$rotation
    yois.loadings <- row.names(loadings)
    if (doDebug) {
      .dfgPca1 <<- dfg
    }
    
    # mm22ss <- ms
    mm22ss <- scale(dfg[, yois.loadings])
    if (doDebug) {
      .mm22ss <<- mm22ss
    }
    okk <- apply(mm22ss, 1, sumna)
    # mm22ss[okk == 0,] <- scores
    if (doScale){
      scoresallTmp <- as.data.frame(as.matrix(scale(dfg[, yois.loadings])) %*% loadings)  
    } else {
      scoresallTmp <- as.data.frame(as.matrix(dfg[, yois.loadings]) %*% loadings)
    }
    dfg.inclscoresTmp <- cbind(dfg, scoresallTmp)
    .dfg.inclscoresTmp <<- dfg.inclscoresTmp
    # now correct
    scoresall <- as.data.frame(as.matrix(ms) %*% loadings)
    if (doDebug) {
      .scoresall <<- scoresall
    }
    dfg.inclscores <- dfg.inclscoresTmp
    dfg.inclscores[okk == 0, names(scoresall)] <- scoresall
    
    if (doPCAonly) {
      attr(dfg.inclscores, "outpca") <- outpca
      attr(dfg.inclscores, "correlmatrix") <- correlmatrix
      return(dfg.inclscores)
    } else {
      outpca$dfg.inclscores <- dfg.inclscores
      outpca$correlmatrix <- correlmatrix
    }
  } # addScores
  
  
  
  if (saveLoading != "none") {
    if (!saveLoading %in% names(kb.local)) {
      kb.local[[saveLoading]] <- list()
    }
    kb.local[[saveLoading]][["outpca"]] <- outpca
  }
  if (cloudKB) {
    if (!"KNOW" %in% search()) {
      message("creating & attaching environment 'KNOW'")
      KNOW <- new.env()
      attach(KNOW, name = "KNOW")
    }
    assign("kb.li", kb.local, pos = "KNOW")
  }
  if (doDebug) {
    .kb.local <<- kb.local
  }
  
  
  
  
  
  if (is.null(outward.loadings))   outward.loadings <- TRUE
  if (is.null(outward.scores))     outward.scores <- TRUE
  just.out <- 1
  if (!outward.loadings)     just.out <- -1
  just.out.scores <- 1
  if (!outward.scores)     just.out.scores <- -1
  if (doDebug) {
    .outpca <<- outpca
    message("loadrad")
    str(loadrad)
    loadrad <- as.numeric(loadrad)
  }
  loads <- loadrad * as.data.frame(loading[, factoraxes])
  pcnames <- paste0("PC", factoraxes)
  names(loads) <- pcnames
  if (doDebug) .scores <<- scores
  if (doDebug) .loads <<- loads
  m <- cbind(m2, scores)
  # if (smoothFactor != "none") 
  #   m <- hstransform(m, yois = names(scores), foi = smoothFactor, 
  #                   FUN = "hssmt5", timevar = timevar)
  if (doDebug) {
    .m3 <<- m
  }
  if (doDebug) {
    .pggargs <<- list(xoi = pcnames[1], yoi = pcnames[2]
                      , group = group
                      , foi = color
                      , geom = geom
                      , psize = psize
                      , lwd = lwd, fsize = fsize
                      , data.oi = data.oi, lang.oi = lang.oi
                      , xlab = xlab, ylab = ylab, title = title
                      , chunkTitle = chunkTitle, allowppt = FALSE
                      , lineAcross = lineAcross, xextra = xextra
                      , focusdfg = FALSE, doplot = FALSE
                      , labelRepel = labelRepel
                      , ...
    )
  }
  labelCol1 <- labelCol
  # & (labelRepel > 0)
  if (is.null(labelCol)) {
    labelCol1 <- "none"
  }
  
  p <- pggs(m, xoi = pcnames[1], yoi = pcnames[2]
            , label = labelCol1
            # , group = c(group, labelCol)
            , group = group
            , foi = color
            , geom = geom
            # , lineType = NULL
            , psize = psize, pointAlpha = pointAlpha
            , lwd = lwd, fsize = fsize, data.oi = data.oi, lang.oi = lang.oi
            , xlab = xlab, ylab = ylab, title = title, chunkTitle = chunkTitle
            , allowppt = FALSE, lineAcross = lineAcross
            , xextra = xextra, focusdfg = FALSE, doplot = FALSE
            , labelRepel = labelRepel
            , ...
  )
  p
  if (grepl("errorbar", geom)) m  <- p$data
  if (doDebug) .p <<- p
  if (doDebug) .m4 <<- m
  if (plotaxes) {
    p <- p + geom_hline(yintercept = 0, size = plotaxes)
    p <- p + geom_vline(xintercept = 0, size = plotaxes)
  }
  if (grepl("arrow", geomLoad)) {
    dfg.loadingArrows <- cbind(loads[, paste0("PC", factoraxes[1:2])] * 
                                 relloadrad, loads[, paste0("PC", factoraxes[1:2])])
    names(dfg.loadingArrows) <- c("x", "y", "xe", "ye")
    p <- p + geom_segment(data = dfg.loadingArrows
                          , aes(x = x,y = y, xend = xe, yend = ye)
                          , arrow = do.call(arrow
                                            , arrowList), size = loadwidth, color = loadcolor)
  }
  if (grepl("label", geomLoad)) {
    loads$angles <- 0
    loads$variable <- row.names(loads)
    if (!is.null(dict)){ 
      loads$xoi.pl <- tl3(loads$variable, #data.oi = data.oi, 
                          lang.oi = lang.oi, dict = dict, key=tl3Key, showSuffix = showSuffix)
    } else {
      loads$xoi.pl <- loads$variable
    }
    loads$xoi.pl <- loadLabelMod(loads$variable)
    
    if (loadingRadial) {
      loads$angles <- 180 * atan2(loads[, pcnames[2]], 
                                  loads[, pcnames[1]])/pi
      loads[loads$angles > 90, "angles"] <- loads[loads$angles > 
                                                    90, "angles"] - 180
      loads[loads$angles < -90, "angles"] <- loads[loads$angles < 
                                                     -90, "angles"] + 180
      loads$vv <- 0.5
    } else {
      loads$vv <- -space + (1 + 2 * space) * (just.out * 
                                                loads[, pcnames[2]] < 0)
    }
    loads$hh <- -space + (1 + 2 * space) * (just.out * loads[, 
                                                             pcnames[1]] < 0)
    loads$hh <- loads$hh + (runif(nrow(loads)) - 0.5) * just.random
    loads$vv <- loads$vv + (runif(nrow(loads)) - 0.5) * just.random
    loads[(seq(nrow(loads))-1) %% showLoadLabelModulo > 0, "xoi.pl"] <- NA
    
    # if (doDebug) {
    .loads2 <<- loads
    # }
    # loads
    getOption("ggrepel.max.overlaps", default = 10)
    # geom_text
    
    p <- p + do.call(loadLabelFun, list(data = loads
                                        , aes_string(x = pcnames[1]
                                                     , y = pcnames[2]
                                                     , angle = "angles"
                                                     , label = "xoi.pl" 
                                                     , hjust = "hh", vjust = "vv")
                                        , size = lsize
                                        , color = loadLabelColor))
  }
  
  if (grepl("point", geomLoad)) {
    p <- p + geom_point(data = loads, aes_string(x = pcnames[1], 
                                                 y = pcnames[2]), size = 2)
  }
  p <- p + theme(plot.title = element_text(lineheight = 4, 
                                           face = "bold", color = "black"))
  
  ################################################# point justification to outside
  if (!is.null(labelCol)) {
    if (labelRepel == 1) {
      if (just.out.scores == "random") {
        hh <- (runif(nrow(m)) - 0.5) * 3
        vv <- (runif(nrow(m)) - 0.5) * 3
      } else {
        hh <- -space + (1 + 2 * space) * (just.out.scores * m[, 
                                                              pcnames[1]] < 0)
        vv <- -space + (1 + 2 * space) * (just.out.scores * m[, 
                                                              pcnames[2]] < 0)
      }
      if (mega) {
        hh <- hh * 0 + 0.5
        vv <- vv * 0 + 0.5
      }
      m$hh <- hh
      m$vv <- vv
      m$zz <- labelSize
      # str(labelCol)
      # setDT(m)
      # setnames(m, labelCol, make.names2(labelCol))
      # labelCol <- make.names2(labelCol)
      # m <- as.data.frame(m)
      # str(m)
      if (doDebug) {
        .m5 <<- m
      }
      m$dist <- sqrt(m[, paste0("PC", factoraxes[1])]^2 + 
                       m[, paste0("PC", factoraxes[2])]^2)
      m[m$dist <= rel.min.dist * max(m$dist), labelCol] <- ""
      p <- p + geom_text(data = m
                         , aes_string(x = pcnames[1]
                                              , y = pcnames[2]
                                              , label = labelCol
                                              , group = foi
                                              , color = color 
                                              , hjust = "hh"
                                              , vjust = "vv")
                         , size = labelSize)
      if (mega) {
        p <- p + geom_text(data = m
                           , aes_string(x = pcnames[1] 
                                                , y = pcnames[2]
                                                , label = labelCol)
                           , size = labelSize, 
                           color = "white", fontface = 2)
      }
    }
  }
  
  if (doDebug) .m.c <<- m
  if (doDebug) .pppca <<- p
  # not used
  ww <- ggplot_build(p)
  xrange <- ww$layout$panel_params[[1]]$x.range
  yrange <- ww$layout$panel_params[[1]]$y.range
  if (yoisaxistype != "none") {
    dfg <- m
    if (doDebug) .dfg2.pca <<- dfg
    if (yoisaxistype == "auto") {
      yoisRange <- apply(dfg[, yoisaxis, drop = FALSE], 2, range, na.rm = TRUE)
      ww <- log(diff(yoisRange))/log(10)
      axisdigits <- as.numeric(3 - floor(ww))
      ww <- ifelse(ww < 1, 10, 0.1)
      ww <- 1
      yoiStep <- 2 * as.numeric(floor(0.5 + ww * diff(yoisRange))/(4 * ww))
      yoiStep <- as.numeric(diff(yoisRange)/3)
      names(yoiStep) <- yoisaxis
    }
    if (yoisaxistype == "range") {
      yoisRange <- apply(dfg[, yoisaxis, drop = FALSE], 2, range, na.rm = TRUE)
      ww <- log(diff(yoisRange))/log(10)
      axisdigits <- as.numeric(3 - floor(ww))
      yoiStep <- as.numeric(diff(yoisRange))
      names(yoiStep) <- yoisaxis
    }
    if (yoisaxistype == "given") {
      yoiStep <- yoiStepgiven
      names(yoiStep) <- yoisaxis
      axisdigits <- axisdigitsgiven
    }
    names(axisdigits) <- yoisaxis
    yoi <- yoisaxis[1]
    for (yoi in yoisaxis) {
      if (yoisaxistype == "range") {
        ax2 <- 0 * dfg[1:2, yoisaxis, drop = FALSE]
        ax2[, yoi] <- yoisRange[, yoi]
      }
      else {
        ax2 <- 0 * dfg[rep(1, 200), yoisaxis, drop = FALSE]
        ax2[, yoi] <- seq(from = 0, by = yoiStep[yoi], length.out = nrow(ax2))
        # ax2c <- ax2
        if (!orangeoutside){
          ax2 <- ax2[ax2[, yoi] <= max(dfg[, yoi], na.rm = TRUE), , drop = FALSE]
          ax2 <- ax2[ax2[, yoi] >= min(dfg[, yoi], na.rm = TRUE), , drop = FALSE]
        }
      }
      ax2s <- ax2
      ax2s[, yoi] <- (ax2s[, yoi] - outpca$center[yoi])/outpca$scale[yoi]
      # ax2b <- as.data.frame(as.matrix(ax2s) %*% (loadrad * outpca$rotation[yoisaxis, , drop = FALSE]))
      ax2b <- as.data.frame(as.matrix(ax2s) %*% outpca$rotation[yoisaxis, , drop = FALSE])
      
      ax2b$label <- format(ax2[, yoi], nsmall = axisdigits[yoi])
      if (orangeoutside){
        ax2b <- ax2b[ax2b[, 1] > xrange[1],]
        ax2b <- ax2b[ax2b[, 1] < xrange[2],]
        ax2b <- ax2b[ax2b[, 2] > yrange[1],]
        ax2b <- ax2b[ax2b[, 2] < yrange[2],]
      }
      
      coef <- coef(lm(as.formula(paste(pcnames[2:1], collapse = "~")), 
                      data = ax2b))
      p <- p + geom_abline(slope = coef[2], color = "orange", size = 0.5)
      # p <- p + geom_path(data = ax2b, aes_string(x = pcnames[1], 
      #                                            y = pcnames[2]), color = loadcolor, size = loadwidth)
      p <- p + geom_point(data = ax2b, aes_string(x = pcnames[1], 
                                                  y = pcnames[2]), fill = "yellow", color = "brown", 
                          size = 4, shape = 22)
      message("axis: ", yoi)
      print(ax2b)
      p <- addText(p, ax2b, pcnames[1], pcnames[2], laboi = "label", 
                   color = "brown", size = 5, fontface = 2, angle = 0, 
                   hjust = -0.2, vjust = 0.5)
    }
  }
  outpca$plot <- p
  if (doDebug) {
    .outpca.0 <<- outpca
  }
  if (doplot) 
    print(p)
  if (nchar(chunkTitle)) {
    # Sys.sleep(2)
    log_debug("pca-plot into rmd")
    vrmd("add", p, chunkId = chunkTitle)
  }
  return(outpca)
}
