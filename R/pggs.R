#' cleanTitle
#' @export
cleanTitle <- function(title){
  title <- paste(title, collapse = "\n")
  if (length(title) == 0){
    title <- waiver()
  } else {
    if (nchar(title) == 0) title <- waiver()
  }
  title
}


#' hour_tz
#' @export
hour_tz <- function(dateTime, tzone = "Europe/Amsterdam", ...){
  current_tz <- attr(dateTime, "tzone")
  if (length(current_tz)){
    log_warn("hour_tz| converting from time zone {current_tz} to {tzone}")
  } else {
    log_warn("hour_tz| no time zone in dateTime")
  }
  hour(lubridate::with_tz(dateTime, tzone = tzone))
}


#' pggsPALETTE
#' @export
pggsPALETTE <- c("black", "darkred", "darkgreen", "darkblue"
                 , "magenta", "orange", "cyan", "violet"
                 , "red", "lightgreen", "lightblue", "gray")


# uses aphTimes aphValues aphVariables aphFactors
# TODO
# p <- ggplot(data=data.table(x=1:33, value = 33:1)) + geom_point(aes(x=x, y=value))
# object.size(p)  # 6kb
# p <- pggs(data.table(x=1:33, value = 33:1))
# object.size( <- )  # 7 Mb

#' pggs
#' 
#' plot with ggplot! (and submit to rmd)
#' 
#' @param dfg = data frame or tibble or data.table
#' @param xoi = character string for variable to put on x-axis
#' @param yoi = character string for variable to put on y-axis
#' @param foi = character string for variable to use as grouping factor (tied to color)
#' @param group = character string for variable to use as grouping factor (NOT tied to color)
#' @param facet_w = character string for variable to use as facetting factor (NOT tied to color)
#' @examples \dontrun{ inst/example/pggsExample01.R }
#' @author Hans.Schepers@@bayer.com
#' @rawNamespace import(ggplot2, except = last_plot)
#' @importFrom grid textGrob rasterGrob gpar grobTree
#' @importFrom shiny reactiveValuesToList isolate
#' @importFrom plotly ggplotly
#' @importFrom lubridate isoweek
#' @export
pggs <- function(dfg
                 , x = NULL, y = NULL
                 , xoi = "none"
                 , yoi = "value"
                 , foi = "none"
                 , group = foi
                 , autoYoi = FALSE
                 , doMelt = FALSE
                 , variable.factor = FALSE
                 , argsMelt = list()
                 , extractElement = "cropLong"
                 , idcol = "scenId"
                 , keep = character(0)
                 , autoKey = TRUE
                 , hsep = " | "
                 , facet_w = "none"
                 , facet_g = NULL, facetcols = NULL, facetrows = NULL
                 , facetlabeller = c("auto", "label_value", "label_both")[1]
                 , facetscales = NULL
                 , free_y = "auto", free_x = FALSE
                 , hash = c("none", "auto")[1]
                 , hiyois = c("temperature", "W")[0]
                 , hiyoisColor = "#ffff11"
                 , hiyoisAlpha = 0.3
                 
                 , factorIgnore = character()
                 , alwaysAtStart = character(0)
                 , lang.oi = "none"
                 , showSuffix = TRUE
                 , addedDict = data.table(en = "test", ee = "UKtest"
                                          , nl = "NLtest", fr = "FRtest")[0]
                 , yearSync = NULL
                 , doAsDate = (!is.null(yearSync))
                 # , planting_week = NULL # supports x axis with weeks to start at say week 40!
                 , continuousFoi = FALSE
                 , zoi = "value", interpolate = FALSE  # for geom "raster"
                 , foiWidth = 3
                 , ggplotly = FALSE
                 , docuCall = FALSE
                 , p = "new" # geom_blank()
                 , more = character(0)
                 , tz.oi = NULL
                 , dateformat.oi = NULL
                 #
                 , chunkBase = character(0)
                 , chunkId = NULL
                 , chartClassAdded = "pggs"
                 , chunkTitle = NULL
                 , tabTitle = NULL
                 , doTabbed = 0
                 , data2layer0 = FALSE
                 , ggtheme = c("aph2", "small", "ytddashboard")[1]
                 , panelBackgroundColor = NULL
                 , panelBorderColor = "grey"
                 , panelBorderSize = 2
                 , margin = c(4, 3, 5, 5)  # c(3, 1, 3, 3)
                 # , tinyID = character(0)
                 , expandX = ggplot2::waiver()
                 , expandY = ggplot2::waiver()
                 , tag = ggplot2::waiver()
                 , caption = ggplot2::waiver()
                 , pxysc = "none" # pxynew is ggplot object to automatically extrax xsc and ysc from (and set xhift possibly)
                 # , xshift = 0
                 
                 , alpha = 0.4  # used for columns
                 , geom = "line"
                 , lwd = 0.5
                 , psize = 1.5
                 , pointSize = NULL
                 , pointColor = NULL
                 , doplot = FALSE
                 , pointAlpha = 0.75
                 , lineAlpha = 1
                 , lineColor = NULL
                 , lineType = 1 # (0 = blank, 1 = solid, 2 = dashed, 3 = dotted)
                 , lineTypeFit = lineType
                 , lineTypeEB = lineType
                 , lwdEB = lwd
                 , lineAlphaEB = lineAlpha
                 , lineAcross = NULL
                 
                 , libbonSize = 1
                 , libbonAlpha = .4
                 , libbonLineType = 1
                 , libbonMinColor = "red"  # for ribbon-lines
                 , libbonMaxColor = "green"
                 , colPosition = "dodge2"
                 , ribbonColor = "blue"
                 , ribbonAlpha = 0.15
                 , yois = character(0)
                 , yoisOUT = character(0)
                 
                 , errorWidth = 0.2
                 , aggfois = NULL
                 
                 , aggrExpression = c("none", "lbub"
                                      , "hsummary(value)"
                                      , "meanValue")[1]
                 , aggrUnit = "none"
                 , aggrBy = c(xoi, foi, facet_w)
                 , aggrAccross = character(0)
                 
                 , dfgRibbon = c("copy", "auto")[1]
                 , ribbonAggrExpression = c("none", "lbub"
                                            , "hsummary(value)"
                                            , "meanValue")[3]
                 , ribbonAggrUnit = "none"
                 , ribbonAggrBy = "all"
                 , ribbonAggrAccross = character(0) # group
                 , beforeRibbonFilterText = "TRUE"
                 , afterRibbonFilterText = "TRUE"
                 
                 # , autoAggr = FALSE
                 
                 , fois2sort = character(0)
                 , foisFixed = character(0)#NULL
                 
                 , yois2sort = character(0)
                 , yois2smooth = NULL, fois2smoothby = "", smoothWindow = 3, smoothIter = 2
                 , wrapFoi = FALSE
                 , wrapFacet = FALSE
                 , ymin = "hmin", ymax = "hmax"
                 # , ymin = "ymin", ymax = "ymax"
                 , xmin = "xmin", xmax = "xmax"
                 , aggyois = unique(c(yoi#, intersect(names(dfg), yois)
                                      , yois2sort, yois2smooth, zoi))
                 #, agg.fun="hmean"
                 #
                 , el.lwd = lwd, lwdFit = 1, lineAlphaFit = 1
                 , fitColor = NULL
                 , el.alpha=NULL, el.fill=FALSE
                 # , fitacross=NULL
                 , annoFit=FALSE, xoiFit = xoi
                 , grid = TRUE
                 , grid.x = grid, grid.y = grid
                 , grid.minor.x = FALSE, grid.minor.y = FALSE
                 , dofit=FALSE, ci.alpha=NULL, eb.alpha=ci.alpha
                 , poly=1, method="lm", formula=paste0("y~poly(x,",poly,",raw=T)")
                 , statgeom="smooth"
                 , weight.expo = 0
                 #
                 , input = NULL, pfx = "ZQZQ"
                 , sleep = 0
                 , fsize = 10
                 , titleSize = NULL, titleVjust = 3
                 , subtitleSize = NULL, subtitleVjust = 3
                 
                 , legend = "top"
                 , suppressLength1Legend = TRUE
                 , legendSize = fsize
                 , legendTitle = c("auto", "none")[1]
                 , doShow_guide = c("fit")[0]
                 , noLegendItem = character(0)
                 
                 , abline=NULL, ablinecolor="blue"
                 , vline=NULL, hline=NULL
                 
                 # , verbatim = c("xlab", "ylab", "title", "subtitle")[0]
                 , xlab = xoi, ylab = yoi
                 , title = NULL, subtitle = NULL
                 , doMath = TRUE
                 , yextra="auto", xextra="auto"
                 , aspect.ratio="auto"
                 , xsc = NULL, xsc_q = NULL
                 , ysc = NULL, ysc_q = NULL
                 , xsc_ysc = 0
                 , xtics = "auto", ytics = "auto", alwaysnewxaxes = TRUE
                 , yticsShift = 0
                 , dateformat = "wk%W\n%d %b"
                 , date_minor_breaks = waiver()
                 # ,datebreaks = "1 month"
                 , datebreaks = NULL
                 # , datebreaks = "2 weeks", dateformat = "wk%W\n%d %b"
                 , tz = Sys.timezone()
                 , minorbreakInterval="4 hours", dateexpand=ggplot2::waiver()
                 
                 , violinTrim = TRUE
                 , violinColor = "black"
                 , violinAdjust = 0.5 # bandwith Smoothing
                 , violinScale = c("width", "count", "equal", "area")[4]
                 , violinAlpha = 0.5
                 , violinDraw_quantiles = c(0.1, 0.5, 0.9)
                 , violinProbs = NULL #c(0.1, 0.5, 0.9)  # extra points
                 , jitterWidth = 0
                 , jitterHeight = 0
                 , jitterColor = "darkgrey"
                 , subset = NULL
                 
                 # labels
                 , projector = TRUE
                 , mega = FALSE
                 , labelMod = function(x, ...) {x}
                 , label = "none", labelSize = 3, labelDigits = NULL, labelColor = "black"
                 , labelnchars = 100
                 , labelPrefix="", labelSuffix="", labelX=NULL, labelY=NULL
                 , labelRepel = 0
                 , labelXjust=-0.1, labelYjust=-0.2, labelXmid=1, labelYmid=1, labelAngle=0
                 , max.overlaps = 20
                 , doLabelVarColor = TRUE
                 # axis
                 , xaxis="auto", yaxis = c("auto", "right")[1]
                 , yaxisNewlines = c(0, 0)
                 , xaxisNewlines = c(0, 0)
                 , xangle=0, yangle=0
                 , xsize=fsize, ysize=fsize, stripSize=fsize
                 , logy=FALSE, logx=FALSE, flip=FALSE
                 
                 , palette.oi = pggsPALETTE
                 , pal.oi = c("bayer")[0]
                 , doublePalette = FALSE
                 , addgrad=FALSE
                 # for export of chart as png
                 , png = NULL, pngdir = "."
                 # for vrmd:
                 , ggwidth = 14, ggheight = 8  # inch
                 , childWidth2 = "100%"
                 , dpi = 96
                 , allowppt = TRUE, addppt = FALSE
                 , slideTitle = title
                 , slideText = "-"
                 , LOGLEVEL = 300
                 , verbosity = 0
                 , whatToReturnUponEmpty = list(ggplot2::ggplot()
                                                , ggplot2::geom_blank()
                                                , NULL)[[1]]
                 , ...){
  require(ggplot2)
  require(logger)
  if (!exists("log_threshold", mode = "function")){
    log_fatal <- function(...) invisible(NULL)
    log_error <- function(...) invisible(NULL)
    log_warn  <- function(...) invisible(NULL)
    log_success<- function(...) invisible(NULL)
    log_info  <- function(...) invisible(NULL)
    log_debug <- function(...) invisible(NULL)
    log_trace <- function(...) invisible(NULL)
    log_threshold <- function(...) structure(LOGLEVEL, level = "INFO", class = c("loglevel", "integer"))
  }
  if (is.null(verbosity)){
    verbosity <- as.numeric(log_threshold())
  }
  # if (verbosity >= 1000){
  #   checkMaskings()
  # }
  if (verbosity >= 625){
    message("print(sys.call()):")
    print(sys.call())
  }
  
  
  # parse input if present #####################################################
  
  if (exists("g.pggsInput") & is.null(input)){
    log_debug("using global object 'g.pggsInput'. .")
    input <- g.pggsInput 
  }
  
  if (length(input)){
    funFormals <- formals(pggs)
    if (!"list" == class(input)[1]) {
      input <- shiny::isolate(shiny::reactiveValuesToList( input ))
    }
    names(input) <- gsub(pfx, "", names(input))
    # ignored <- base::setdiff(names(input), names(funFormals))
    common <- intersect(names(input), names(funFormals))
    input <- input[common]
    
    CALL <- match.call()
    for (nm in names(input)) {
      if (!nm %in% names(CALL)) {  # don't use if specified in Call
        assign(nm, input[[nm]])
      }
    }
  }
  .last_pggsCALL <<- match.call()
  
  
  # check data #################################################################
  
  if (missing(dfg)) {  #!exists("dfg")
    log_warn("no dfg found, continuing. .")
    if  (!is.null(x) & !is.null(y)){
      dfg <- data.frame(x = x, y= y)
    } else {
      return(whatToReturnUponEmpty) #ggplot2::geom_blank())
    }
    # dfg <- data.table(dateTime=0, value = 1, label = ".")[0]
  }
  
  if (inherits(dfg, "matrix")) {
    dfg <- as.data.frame(dfg)
  }
  
  if(doMelt) {
    argsMelt <- mergeParameters(argsMelt
                                , list(DT = dfg
                                       , variable.factor = variable.factor))
    dfg <- do.call(aphMelt, argsMelt)
    if (autoKey){
      aphKey(dfg, alwaysAtStart = alwaysAtStart, factorIgnore = factorIgnore)
    }
  }
  if(doMelt | length(yoisOUT)) {
    if (!length(yois)) {
    yois <- aphVariableLevels(dfg)
    }
  }
  
  if(length(subset)) {
    dfg <- as.data.frame(as.data.table(dfg[eval(subset)]))
  }
  
  if (inherits(dfg, "list")){
    if (extractElement %in% names(dfg[[1]])) {
      log_info("reading dfg as a list, extracting element '{extractElement}', with idcol = '{idcol}'")
      # dfg is a list, named or unnamed, of df's or dt's
      nms <- names(dfg)
      if (is.null(nms)) {
        names(dfg) <- paste0("sim", seq_along(dfg))
      }
      
      dfg <- as.data.frame(
        rbindlist(lapply(lapply(dfg, getElement, extractElement), as.data.table)
                  , idcol = idcol)
      )
    }
  }
  
  
  if (is.null(dfg)) {
    log_warn("dfg is NULL")
    return(whatToReturnUponEmpty)
  }
  
  if (!inherits(dfg, "data.frame")) {
    log_warn("forcing your input into a data.frame. .")
    if (inherits(dfg, "R6")) {
      dfg <- as.data.frame(as.data.table(dfg))
    } else {
      dfg <- as.data.frame(as.numeric(as.character(dfg)))
      names(dfg) <- LETTERS[1:ncol(dfg)]
      dfg$x <- seq(nrow(dfg))
      xoi <-  "x"
      yoi <- names(dfg)[1]
    }
    # TODO use names of a matrix if present?
  }
  
  if (!nrow(dfg)) {
    log_warn("dfg has no rows")
    return(whatToReturnUponEmpty)
  }
  
  if (length(yois)) {
    keep <- union(keep, "processName")
  }
  # if (!is.null(planting_week)){
  #   foisFixed <- union(foisFixed, "wk")
  # }
  if (length(foisFixed)) {
    keep <- union(keep, foisFixed)
  }
  
  
  if (!exists("g.doplot")) { g.doplot = FALSE  ; g.doplot <<- g.doplot }
  if (!exists("g.addppt")) { g.addppt = FALSE ; g.addppt <<- g.addppt }
  if (!exists("g.addRmd")) { g.addRmd = FALSE ; g.addRmd <<- g.addRmd }
  if (!exists("g.reqCnk")) { g.reqCnk = TRUE  ; g.reqCnk <<- g.reqCnk }
  if (!exists("g.ggly")) { g.ggly = FALSE  ; g.ggly <<- g.ggly }
  if (exists("g.chunkBase")) { if(!length(chunkBase)) chunkBase = g.chunkBase }
  
  
  if (inherits(chunkTitle, "Date")) chunkTitle <- as.character(chunkTitle)
  if (inherits(chunkBase,  "Date")) chunkBase  <- as.character(chunkBase)
  if (inherits(chunkId,    "Date")) chunkId    <- as.character(chunkId)
  if (inherits(tabTitle,   "Date")) tabTitle   <- as.character(tabTitle)
  if (!is.null(pointSize)){
    psize <- pointSize
  }
  
  if ("wk" %in% c(xoi, yoi, foi, group, facet_w, facet_g, keep, label)){
    if (!"wk" %in% names(dfg)){
      dfg <- addTimeRes(dfg, "wk")
    }
  }
  if ("yr" %in% c(xoi, yoi, foi, group, facet_w, facet_g, keep, label)){
    if (!"yr" %in% names(dfg)){
      dfg <- addTimeRes(dfg, "yr")
    }
  }
  
  if ("mon" %in% c(xoi, yoi, foi, group, facet_w, facet_g, keep, label)){
    if (!"mon" %in% names(dfg)){
      dfg <- addTimeRes(dfg, "mon")
    }
  }
  
  if ("quarter" %in% c(xoi, yoi, foi, group, facet_w, facet_g, keep, label)){
    if (!"quarter" %in% names(dfg)){
      log_debug("column `quarter`is not found in dfg, but needed?! - making based on dateTime, local_time or dDate?!")
      if ("dateTime" %in% names(dfg)){
        dfg$quarter <- lubridate::quarter(dfg$dateTime)
      }
      if ("local_time" %in% names(dfg)){
        dfg$quarter <- lubridate::quarter(dfg$local_time)
      }
      if ("dDate" %in% names(dfg)){
        dfg$quarter <- lubridate::quarter(dfg$dDate)
      }
    }
  }
  
  if ("hr" %in% c(xoi, yoi, foi, group, facet_w, facet_g, keep, label)){
    if (!"hr" %in% names(dfg)){
      log_debug("column `hr`is not found in dfg, but needed?! - making based on dateTime, local_time?!")
      if ("local_time" %in% names(dfg)){
        dfg$hr <- lubridate::hour(dfg$local_time)
      }
      if ("dateTime" %in% names(dfg)){
        dfg$hr <- lubridate::hour(dfg$dateTime)
      }
    }
  }
  
  if (is.null(foi)) foi <- "none"
  if (is.null(group)) group <- "none"
  group <- union(group, foi)  # TOCHECK new 20220829
  if (is.character(lineType)){
    # str(dfg)
    # message(424)
    setDT(dfg)
    dfg[, lineType := as.character(lineType)]
    group <- union(group, lineType)  # TOCHECK new 20230610
  } 
  
  if (length(geom) > 1){
    log_debug("collapsing geom elements")
    geom <- paste(geom, collapse = "")
  }
  # groupsNotFound <- setdiff(group, intersect(names(dfg))
  group <- intersect(names(dfg), group)
  if (!length(group)) group <- "none"
  yoiOrig <- yoi
  groupOrig <- group
  if (length(xoi) > 1)       {dfg <- haddKey(dfg, xoi,        sep = hsep, keyID="xoiKey")     ; xoi <- "xoiKey"}
  if (length(yoi) > 1)       {dfg <- haddKey(dfg, yoi,        sep = hsep, keyID="yoiKey")     ; yoi <- "yoiKey"}
  if (length(label) > 1)     {dfg <- haddKey(dfg, label,      sep = hsep, keyID="labelKey")   ; label <- "labelKey"}
  if (length(foi) > 1)       {dfg <- haddKey(dfg, foi,        sep = hsep, keyID="foiKey")     ; foi <- "foiKey"  }
  if (length(facet_w) > 1)   {dfg <- haddKey(dfg, facet_w,    sep = hsep, keyID="facet_wKey") ; facet_w <- "facet_wKey"}
  # if (length(facet_g) > 1)   {dfg <- haddKey(dfg, facet_g,    sep = hsep, keyID="facet_gKey") ; facet_g <- "facet_gKey"}
  if (length(group) > 1)     {dfg <- haddKey(dfg, group,      sep = hsep, keyID="groupKey")   ; group <- "groupKey"}
  # if (length(labelColor) > 1){dfg <- haddKey(dfg, labelColor, sep = hsep, keyID="labelColor") ; labelColor <- "labelColor"}
  
  aggyois <- setdiff(aggyois, yoiOrig)
  aggyois <- union(aggyois, yoi)
  
  xoiPrefs <- rev(aphTimes(dfg))           # c("dateTime", "Index", "doy")
  if ("dateTime" %in% xoiPrefs){
    xoiPrefs <- unique(c("dateTime", xoiPrefs))
  }
  if ("local_time" %in% xoiPrefs){
    xoiPrefs <- unique(c("local_time", xoiPrefs))
  }
  xoiPrefs <- xoiPrefs[1]
  
  yoiPrefs   <- aphValues(dfg)[1]               # "value"
  facetPrefs <- aphVariables(dfg)               # c("processName", "variable", "observation.name")
  foiPrefs   <- c(aphFactors(dfg), facetPrefs)  # c("differentiator", "plot_name", "cycle_name", "field_name", "account_name", "account_id")
  if (!is.null(xoi))     if (xoi     == "none") {xoi     <- c(xoiPrefs, "none")}
  if (!is.null(yoi))     if (yoi     == "none") {yoi     <- c(yoiPrefs, "none")[1]}
  
  xoi <- xoi[1]
  yoi <- yoi[1]
  
  if (!is.null(facet_w)) if (facet_w == "none") {facet_w <- c(facetPrefs, "none")[1]}
  if (facet_w == "nothing") {facet_w <- "none"}
  if (is.na(facet_w)) {facet_w <- "none"}
  
  if (!is.null(foi))     if (foi[1]  == "none") {foi     <- setdiff(c(foiPrefs, "none"), facet_w)[1]}
  # foi <- setdiff(foi, label)
  if (!length(foi))         {foi <- "none"}
  if (is.na(foi))           {foi <- "none"}
  if (foi[1]  == "nothing") {foi <- "none"}
  
  if(verbosity > 550) .dfg1.gg <<- dfg
  dfg <- as.data.frame(dfg)
  
  if (grepl("date", xoi, ignore.case = TRUE)){
    if (doAsDate){
      message("forcing x-axis variable to date ")
      # dfg <- as.data.frame(dfg)
      dfg[, xoi] <- as.Date(dfg[, xoi])
    }
    if (!is.null(yearSync)){
      if (!is.na(yearSync)){
        message("Syncing year to ", yearSync)
        dfg <- as.data.frame(dfg)
        # .dfgPre00 <<- dfg
        
        oclass <- class(dfg[, xoi])
        # print(oclass)
        dfg[, xoi] <- syncYear(dfg[, xoi], yearSync)
        nclass <- class(dfg[, xoi])
        # print(nclass)
        # .dfgPre0 <<- dfg
        class(dfg[, xoi]) <- oclass
        
        # .dfgPre <<- dfg
        # if(autoKey) {  # alreadydone, later with flag autoKey
        dd <- as.data.table(dfg)
        aphKey(dd
               # , alwaysAtStart = alwaysAtStart
               , factorIgnore = factorIgnore)
        dfg <- as.data.frame(dd)
        # }
        # .dfgPost <<- dfg
      }
    }
  }
  if (!is.null(tz.oi) | !is.null(dateformat.oi)){
    # dateformat.oi = "%H:%M"
    # message("xoi: ", xoi)
    # .dfg000 <<- dfg
    current_tz <- attr(dfg[[xoi]], "tzone")
    if (length(current_tz)){
      # if (current_tz != "UTC"){
        log_warn("converting from tz {current_tz} to {tz.oi}")
      # }
    } else {
      log_warn("strange.. while converting from tz {xoi} to {tz.oi}")
      # str(dfg)
    }
    more <- c(more, paste0('scale_x_datetime(labels = function(x) format(x, "'
                   , dateformat.oi
                   ,'", tz = "'
                   , tz.oi ,'"))'))
  }
  
  
  
  
  
  if (wrapFacet){
    dfg[, facet_w] <- wrapCamel(dfg[, facet_w])
  }
  
  if (wrapFoi){
    dfg[, foi] <- wrapCamel(dfg[, foi])
  }
  
  
  
  log_trace("xoi = {xoi}")
  log_trace("yoi = {yoi}")
  log_trace("foi = {foi}")
  log_trace("group = {group}")
  if (!is.null(facet_g)) {
    log_trace("facet_g = {facet_g}")
  }
  if (mode(all.equal.character(facet_w, foi)) == "logical"){
    foi <- "none"
  }
  
  
  if (!xoi %in% names(dfg)) {
    if (!grepl("(ribbon)|(errorbar)|(errorhbar)", geom)) {
      log_fatal("xoi {xoi} not found")
      return(p)
    }
  }
  
  if (!yoi %in% names(dfg)) {
    # print(sys.call())
    if (!grepl("(ribbon)|(errorbar)|(errorhbar)", geom)) {
      if (autoYoi) {
        log_info("yoi set to first numeric column: {yoi}")
        isnum <- sapply(dfg, is.numeric)
        yoi <- setdiff(names(isnum)[isnum], c(xoiPrefs, xoi))[1]
      }
    } else {
      if (!length(yoi)){
        log_fatal("yoi {yoi} not found")
        return(p)
      }
    }
  }
  
  
  ############################################# formatting / lay out ##########
  yaxisPosition <- "left"
  if(yaxis == "right")  {
    yaxisPosition <- "right"
  }
  
  if (!is.null(ylab)) if (ylab[1] %in% c("value", "hmean", "mean")) ylab <- NULL
  if (!is.null(xlab)) {
    if (xlab[1] %in% c("DAP")) xlab <- "Days after Planting"
    if (xlab[1] %in% c("WAP")) xlab <- "Weeks after Planting"
    if (xlab[1] %in% c("isowk")) xlab <- "Calendar Week"
    if (xlab[1] %in% c("dateTime", "local_time")) xlab <- NULL #"Date - Time"
  }
  
  
  if (projector) {
    if (psize == 2) psize <- 3
    if (fsize == 10) fsize <- 12
    if (lwd == 0.5) lwd <- 1
    # if (isTRUE(nchar(xlab) > 0))  xlab <- capitalise(spaceCamel(xlab))
    # if (isTRUE(nchar(xlab) > 0))  xlab <- capitalise(spaceCamel(xlab))
  }
  
  
  if (ggtheme == "ytddashboard"){
    # fsize = 14
    if (is.null(panelBackgroundColor)){
      panelBackgroundColor = switch(yoi 
                                    , yield.cu = "lightyellow"
                                    , yield.cu.ce = "yellow"
                                    , "lightgrey"
      )
    }
    # panelBorderColor = "red"
    # panelBorderSize = 4
    # margin = c(4, 3, 5, 5)
  }
  
  if (mega) {
    if (psize == 1.5) psize = 8
    if (labelSize == 5) labelSize = 4
    if (labelColor == "black") labelColor = "white"
    if (is.null(labelDigits)) labelDigits = 1
    doLabelVarColor <- FALSE
    labelXjust = 0.5
    labelYjust = 0.5
  }
  
  # xoi <- force(xoi)  
  # yoi <- force(yoi)  
  # foi <- force(foi)  
  if (!xtics %in% c("auto", "months", "maanden")) xtics = as.numeric(xtics)
  if (!ytics %in% c("auto", "hedonic", "sensory")) ytics = as.numeric(ytics)
  
  if (is.null(chunkTitle)){
    if (!is.null(title)) {
      if ("y~x" %in% title){
        title <- makeFormula(yoi, xoi)
      }
    }
  }
  
  if (!is.null(chunkTitle)){
    if ("y~x" %in% chunkTitle){
      log_trace("chunkTitle <- makeFormula(yoi, xoi)")
      chunkTitle <- makeFormula(yoi, xoi)
    }
    
    
    if (is.null(tabTitle)){
      # to keep chunkId short, in case it is a tab name inside vrmd(). .
      names(chunkTitle) <- chunkTitle
    } else {
      
      if (!is.null(names(chunkTitle))){
        # to keep chunkId short, in case it is a tab name inside vrmd(). .
        # chunkTitle <- setNames(tabTitle, names(chunkTitle))
        chunkTitle <- setNames(names(chunkTitle), tabTitle)
        chunkId <- chunkTitle
      }
    }
    if (is.null(chunkId)) {
      chunkId <- paste(chunkBase, chunkTitle)
      # chunkId <- paste(chunkBase, names(chunkTitle))
    }
    
    
  } else {
    
    if (g.addRmd & !g.reqCnk) chunkId <- paste0(chunkBase, htimestamp())
    
  }
  
  if(verbosity > 550) .title <<- title
  if (is.null(title)) {
    if (!is.null(names(chunkTitle))){
      title <- paste(chunkBase, names(chunkTitle))
    } else {
      title <- paste(chunkBase, chunkTitle)
    }
    if(verbosity > 550) .title1 <<- title
  }
  
  
  if (doMath){
    if (isTRUE(nchar(xlab) > 0))  xlab <- mathLabels(xlab)
    if (isTRUE(nchar(ylab) > 0))  ylab <- mathLabels(ylab)
    .titleML <<- title
    if (isTRUE(nchar(title) > 0)) title <- mathLabels(title)
    if (isTRUE(nchar(subtitle) > 0)) subtitle <- mathLabels(subtitle)
    # } else {
    #   xlab <- capitalise(spaceCamel(xlab))
    #   ylab <- capitalise(spaceCamel(ylab))
  }
  
  
  ################################################ facet_w
  if (!is.null(facet_g)) {
    if (!as.character(facet_g)[1] %in% c("nothing", "none")){
      if (facetlabeller == "auto"){
        # facetlabeller <- "label_both"
        facetlabeller <- "label_value"
      }
    }
  }
  
  if (!is.null(facet_w)) {
    if (!facet_w %in% names(dfg)) {
      if (!facet_w %in% c("nothing", "none")){
        log_warn("facet_w not found in dfg: {facet_w}")
        facet_w <- "none"
      }
    }
  }
  if (!is.null(facet_w)) {
    if (!facet_w %in% c("nothing", "none")){
      if (is.numeric(dfg[, facet_w])) {
        if (facetlabeller == "auto"){
          facetlabeller <- "label_both"
        }
        log_trace("fixing facet_w to be non-numeric")
        dfg[, facet_w] <- format(dfg[,facet_w], width = foiWidth)
      }
      if (facetlabeller == "auto"){
        facetlabeller <- "label_value"
      }
    }
    log_trace("facetlabeller: {facetlabeller}")
  }
  
  
  ################################################ foi
  if (!is.null(foi)) {
    if (!foi %in% names(dfg)) {
      if (!foi %in% c("nothing", "none")){
        log_warn("foi not found in dfg: {foi}")
        foi <- "none"
      }
    }
  }
  if (!is.null(foi)) {
    if (!foi %in% c("nothing", "none")){
      if ((is.numeric(dfg[, foi]) | inherits(dfg[, foi], "Date"))   ) {
        if (legendTitle == "auto"){
          legendTitle <- "pleaseDo"
        }
        if (!continuousFoi){
          if (!all(foi == xoi, length(ci.alpha) > 0)){ #zz 20240208
            log_trace("fixing foi to be non-numeric")
            dfg[, foi] <- format(dfg[,foi], width = foiWidth)
          }
        }
      }
      if (legendTitle == "auto"){
        legendTitle <- "none"
      }
    }
    log_trace("legendTitle: {legendTitle}")
  }
  
  # c("drs", "sem", "black", "excelcolors", "traffic4", "pal.std", "custom")
  # geom_path <- geom_line
  # log_trace("gg17")
  # print(facet_w)
  
  
  # prepare label ##########################################################
  labelorig = label
  #   log_trace("labelorig",labelorig,"------------------------------------------------")
  label = "label"
  if (nrow(dfg)){
    if (isTRUE(labelorig[1] == "none") | (!length(labelorig))) {
      if (!length(labelorig)) labelorig <- "none"
      dfg[, label] <- " "
      labelaggfois <- "label"
    } else {
      dfg[, label] <- dfg[,labelorig[1] ]
      if (length(labelorig) > 1) dfg <- haddKey(dfg, labelorig, sep = hsep, keyID="label")  #qqq
      labelaggfois <- "label"
      if (is.numeric(dfg[,labelorig[1] ])) {
        if (!is.null(labelDigits)){
          log_debug("rounding numeric labels")
          dfg[, label] <- round(dfg[, label], digits = labelDigits)
        }
        
        labelaggfois <- NULL
        aggyois <- unique(c(aggyois, label))
      }
    }
  } else {
    labelaggfois <- NULL
    labelorig <- "none"
  }
  #TODO check label vs aggregation compat
  
  if (group[1]   == "none") group   = ".timeStamp"
  if (foi[1]     == "none") foi     = ".timeStamp"
  if (facet_w[1] == "none") facet_w = ".timeStamp"
  if (missing(group)) group = ".timeStamp"
  if (missing(foi))     foi = ".timeStamp"
  if (missing(facet_w)) foi = ".timeStamp"
  if (missing(group)) group = ".timeStamp"
  
  if (all(".timeStamp" == c(facet_w, foi, group))) legend = "none"
  if (!is.null(facet_w)) {
    if (facet_w[1] %in% c("none", ".timeStamp")) facet_w = NULL
  }
  if (suppressLength1Legend){
    if (foi[1] != ".timeStamp"){
      if (length(unique(dfg[, foi])) == 1 & !is.null(facet_w)) legend = "none"
    }
  }
  log_trace("foi after suppressLength1Legend = {foi}")
  log_trace("legend after suppressLength1Legend = {legend}")
  
  if (!group[1] %in% names(dfg)){
    log_trace("group not found in names(dfg), putting to {foi}")
    group <- foi
  }
  if (foi[1] == ".timeStamp") {
    foi <- NULL
    # group <- yoi
  }
  # if (group[1] == ".timeStamp") group <- foi
  if (nrow(dfg)) dfg[, ".timeStamp"] <- to8601()
  
  
  
  if (is.character(psize)){
    aggyois <- unique(c(aggyois, psize))
  }
  
  # aggregate, sort & fix over fois #######################################################
  if (is.character(lwd)){
    aggfois <- union(lwd, aggfois)
    # aggfois <- union("lineType", aggfois)
    # aggfois <- union("lineAlpha", aggfois)
  }
  if (is.character(lineType)){
    aggfois <- union(lineType, aggfois)
  }
  if (is.character(lineAlpha)){
    aggfois <- union(lineAlpha, aggfois)
  }
  fois <- intersect(names(dfg), c(foi, group, groupOrig # groupOrig added 20211219
                                  , fois2smoothby
                                  , facet_w
                                  , all.vars(as.formula(facet_g))  # newly added 20211123 
                                  , aggfois, foisFixed, labelaggfois, ribbonColor))  #xoi
  
  aggyois <- base::setdiff(aggyois, c(fois, aggfois, foisFixed, labelaggfois))
  if(verbosity > 550) .aggyois <<- aggyois
  
  
  if (!yoi[1] %in% names(dfg)){
    yoi <- aphValues(dfg)[1]
    stopifnot(length(yoi) > 0)
  }
  
  if (!all(c(xoi, fois, aggyois) %in% union(zoi, names(dfg)))){
    mmm <- setdiff(c(xoi, fois, aggyois), names(dfg))
    log_error("variable mismatch: missing are: {mmm}")
    log_warn("proceeding with yoi = {yoi}")
  }
  
  
  
  aggrBy = c(xoi, fois, keep, facet_w)
  if(verbosity > 550) .dfgBeforeAggr <<- copy(as.data.table(dfg))
  dfg <- aphAggregate(as.data.table(dfg)
                      , dateCol = xoi
                      , unit = aggrUnit
                      , by = aggrBy
                      , accross = aggrAccross
                      , expr = aggrExpression  )
  dfg <- as.data.frame(dfg)
  if(verbosity > 550) .dfgAfterAggr <<- copy(as.data.table(dfg))
  
  
  
  
  
  ##############################################################################
  if(grepl("error|ibbon|medal", geom)){
    if (!any(c("hmin", "ymin", "min", "max", "lb", "lower", "lowerBound", "lowerBound") %in% 
             names(dfg))){
      ribbonAggrExpression <- "hsummary(value)"
    }
  }
  
  
  if (grepl("ibbon", geom)){
    if (!inherits(dfgRibbon, "data.frame")){
      if (inherits(dfgRibbon, "character")){
        
        if(verbosity > 550) .dfgRibbonCHAR0 <<- dfgRibbon
        if (dfgRibbon[1] == "auto"){
          dfgRibbon <- as.data.table(dfg)
          dfgRibbon <- dfgRibbon[eval(parse(text = beforeRibbonFilterText))]
          
          dfgRibbon <- aphAggregate(
            dfgRibbon
            , unit = ribbonAggrUnit # "none"
            , by = ribbonAggrBy # unique(c(foi, facet_w, group))
            , accross = ribbonAggrAccross
            , expr = ribbonAggrExpression
          )
        } else {
          if (dfgRibbon[1] == "copy"){
            dfgRibbon <- as.data.table(dfg)
          }
        }
        if(verbosity > 550) .dfgRibbonCHAR <<- copy(dfgRibbon)
      }
    }
    if(autoKey) {  # alreadydone, later with flag autoKey
      aphKey(dfgRibbon
             # , alwaysAtStart = alwaysAtStart
             , factorIgnore = factorIgnore)
    }
    # str(dfgRibbon)
    if(verbosity > 550) .dfgRibbon <<- copy(dfgRibbon)
  }
  
  if(verbosity > 550) .dfgBeforeFilters <<- as.data.table(dfg)
  
  if (length(yois)){
    yois <- setdiff(yois, yoisOUT)
    # print(yois)
    afterRibbonFilterText <- setdiff(afterRibbonFilterText, "TRUE")
    afterRibbonFilterText <- union(afterRibbonFilterText
                                   , paste0("processName %in% c(\""
                                            , paste(yois, collapse = "\", \"")
                                            , "\")" ))
    # message(afterRibbonFilterText)
  }
  if (length(afterRibbonFilterText)){
    setDT(dfg)
    for (filterText in afterRibbonFilterText){
      if (filterText != "TRUE"){
        log_trace("parsing afterRibbonFilterText filters: {filterText}")
      }
      # str(dfg)
      dfg <- dfg[eval(parse(text = filterText))]
    }
    setDF(dfg)
  }
  
  # if (autoKey){
  #   aphKey(dfg, alwaysAtStart = alwaysAtStart, factorIgnore = factorIgnore)
  # }
  
  if (length(foisFixed)){
    dfg <- as.data.table(dfg)
    dfg[, (foisFixed) := fixFactor(mget(foisFixed))]
    dfg <- as.data.frame(dfg)
    .dfgFF <<- dfg
  }
  
  # dfg <- fixFactorDF(dfg, foisFixed)
  
  if(verbosity > 550) .dfgBeforeGgplot <<- dfg
  setDF(dfg)
  
  
  
  
  if(grepl("error|ibbon|medal", geom)){
    nms <- c(names(dfgRibbon), names(dfg))
    # message("nms")
    # str(nms)
    # str(ymin)
    if (!ymin %in% nms){
      ymin <- intersect(c(nms, "hmin"), c("hmin", "ymin", "min", "lb", "lower", "lowerBound", "lowerBound"))[1]
      log_debug("changed ymin to {ymin}")
    }
    if (!ymax %in% nms){
      ymax <- intersect(c(nms, "hmax"), c("hmax", "ymax", "max", "ub", "upper", "upperBound", "upperBound"))[1]
      log_debug("changed ymax to {ymax}")
    }
  } else {
    ymin <- character(0)
    ymax <- character(0)
  }
  
  xoi <- xoi[1]
  # yoi <- rev(yoi)[1]
  
  .fois <<- fois
  if (!"all" %in% keep) {
    dfg <- dfg[, intersect(names(dfg)
                           , c(xoi, fois, aggyois, yoi
                               # , "hmean"
                               , keep
                               , ribbonAggrBy
                               , fois2sort, yois2sort
                               , yois2smooth, fois2smoothby
                               , ymin, ymax, xmin, xmax))]
  }
  
  
  # TODO / CHECK
  if (length(c(fois2sort, yois2sort))){
    log_trace("pggs line 269: sorting. .")
    dfg <- as.data.frame(setorderv(as.data.table(dfg)
                                   , c(fois2sort, yois2sort)))
    # .dfg987 <<- dfg
    dfg <- fixFactorDF(dfg, aphFactors(dfg))
    # .dfg989 <<- dfg
  }
  
  if(length(yois2smooth)){
    dd <- as.data.table(dfg)
    # ysmoi <- yois2smooth[1]
    # ysmoi %in% names(dd)
    # names(dd)
    for (ysmoi in yois2smooth){
      # str(dd)
      bak <- dd[, ..ysmoi]
      log_trace("smoothing ", ysmoi)
      # print(dd[,.(range(base::get(yoi), na.rm=TRUE))])
      dd[, c(ysmoi) := list(hfrollmean(.SD[, base::get(ysmoi)]
                                       , n = smoothWindow, reps = smoothIter))
         , by = c(fois2smoothby)]
      # print(dd[,.(range(base::get(yoi), na.rm=TRUE))])
      if (is.na(sum(unlist(dd[, .(range(base::get(yoi), na.rm=TRUE))])))){
        log_trace("Smoothing failed, reverting..")
        dd[, c(ysmoi) := list(unlist(bak)) ]
      }
    }
    dfg <- as.data.frame(dd)
  }
  
  if(length(yois2smooth)){
    if (inherits(dfgRibbon, "data.table")){
      log_trace("Smoothing  Ribbon")
      for (ysmoi in yois2smooth){
        bak <- dfgRibbon[, ..ysmoi]
        dfgRibbon[, c(ysmoi) := list(hfrollmean(.SD[, base::get(ysmoi)], reps = smoothWindow))
                  , by = c(fois2smoothby)]
        
        if (is.na(sum(hrange(dfgRibbon[[yoi]])))){
          log_trace("Smoothing of Ribbon failed, reverting..")
          dfgRibbon[, c(ysmoi) := list(unlist(bak)) ]
        }
      }
    }
  }
  
  
  if ("quarters" %in% datebreaks){
    datebreaks = seq.POSIXt(ISOdatetime(2010, 1, 2, 0,0,0)
                            , ISOdatetime(2030, 1, 2, 0,0,0)
                            , "1 quarters"
                            # ,"3 months"
    )
    dateformat <- "%b"
    # print(class(dfg[, xoi]))
    # ww <- c("POSIXct", "POSIXt")
    if (inherits(dfg[, xoi], "Date")){
      dfg[, xoi] <- as.POSIXct(dfg[, xoi])
    }
  }
  
  if(verbosity >= 550) .dfg.gg <<- as.data.table(dfg)
  # dfg <- groupTailLevels(dfg, foi)
  
  # start of ggplotting ##########################################################
  
  if(verbosity >= 650) {
    log_info("final xoi {xoi}")
    log_info("final yoi {yoi}")
    log_info("final foi {foi}")
    log_info("final group {group}")
    log_info("final facet_w {facet_w}")
    log_info("final facet_g {facet_g}")
  }
  if (free_y == "auto"){
    free_y <- isTRUE(grepl("processName", facet_w)) | 
      isTRUE(grepl("processName", facet_g)) |
      isTRUE(grepl("variable", facet_w)) |
      isTRUE(grepl("variable", facet_g))
  }
  
  themeAPH <- switch(tolower(ggtheme)
                     # ytddashboard
                     , ytddashboard = ggplot2::theme_bw(base_size = fsize) + 
                       ggplot2::theme(panel.background = ggplot2::element_rect(fill = panelBackgroundColor
                                                                               , color = panelBorderColor
                                                                               , linewidth = panelBorderSize)) +
                       ggplot2::theme(axis.title.x = ggplot2::element_text(vjust = grid::unit(-5, "char"))) +
                       ggplot2::theme(axis.title.y = ggplot2::element_text(angle=90, vjust = 5)) +
                       ggplot2::theme(plot.title = ggplot2::element_text(size=15, vjust=3)) +
                       ggplot2::theme(plot.margin = grid::unit(margin, "char"))
                     
                     # small
                     # , small = ggplot2::theme()
                     , aph = ggplot2::theme_bw(base_size = fsize) + 
                       ggplot2::theme(panel.background = ggplot2::element_rect()) + 
                       ggplot2::theme(plot.title = ggplot2::element_text(size=15, vjust=3)) +
                       ggplot2::theme(plot.margin = grid::unit(margin, "char")) +
                       ggplot2::theme(axis.title.x = ggplot2::element_text(vjust = grid::unit(-5, "char"))) +
                       ggplot2::theme(axis.title.y = ggplot2::element_text(angle=90, vjust = 5))# +
                     # ggplot2::theme(panel.border = ggplot2::element_rect(color = "darkgrey", size = 0.5))
                     
                     # aph2
                     , aph2 = ggplot2::theme_bw(base_size = fsize) + 
                       ggplot2::theme(panel.border = ggplot2::element_rect(color = "darkgrey", linewidth = 0.5))
                     
                     # default: 
                     , ggplot2::theme_bw(base_size=fsize) +
                       ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank()) +
                       ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank()) + 
                       ggplot2::theme(panel.border = ggplot2::element_blank())
  )
  if (grepl("raster", geom)){
    themeAPH <- themeAPH +
      ggplot2::theme(panel.border = ggplot2::element_rect(color = "black", fill=NA, size=1))
  }
  
  if (!grid.x)       themeAPH <- themeAPH + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank() )
  if ( grid.x)       themeAPH <- themeAPH + ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color="lightgray", linewidth=.5) )
  if (!grid.minor.x) themeAPH <- themeAPH + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank() )
  if ( grid.minor.x)  themeAPH <- themeAPH + ggplot2::theme(panel.grid.minor.x = ggplot2::element_line(color="lightgray", linewidth=.25) )
  if (!grid.y)       themeAPH <- themeAPH + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank() )
  if ( grid.y)       themeAPH <- themeAPH + ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color="lightgray", linewidth=.5) )
  if (!grid.minor.y) themeAPH <- themeAPH + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank() )
  if (grid.minor.y)  themeAPH <- themeAPH + ggplot2::theme(panel.grid.minor.y = ggplot2::element_line(color="lightgray", linewidth=.25) )
  
  if (!is.null(titleSize)){
    themeAPH <- themeAPH + 
      ggplot2::theme(plot.title = ggplot2::element_text(
        size = titleSize
        , vjust = titleVjust))
  }
  if (!is.null(subtitleSize)){
    themeAPH <- themeAPH + 
      ggplot2::theme(plot.subtitle = ggplot2::element_text(
        size = subtitleSize
        , vjust = subtitleVjust))
  }
  
  # if (!is.null(xsc)) if ((xtics[1] == "auto") & (diff(range(xsc)) < 11) & (abs(hmean(xsc)) < 100)) xtics = 1
  # if (!is.null(ysc)) if ((ytics[1] == "auto") & (diff(range(ysc)) < 11) & (abs(hmean(ysc)) < 100)) ytics = 1
  
  if (lang.oi != "none"){
    # lang.oi <- "ee"
    if (!inherits(addedDict, "data.frame")){
      if (!inherits(addedDict, "list")){
        addedDict <- as.list(addedDict)
      }
      addedDict <- as.data.table(addedDict)
    }
    if (nrow(addedDict) > 0 | !exists("hi18n", envir = .GlobalEnv)){
      log_warn("creating a new hi18n..!")
      if (exists("hi18n", envir = .GlobalEnv)){
        hi18n.b <<- hi18n
      }
      hi18n <- starti18n(lang.oi = lang.oi, addedDict = addedDict)
      hi18n <<- hi18n
    }
    if (lang.oi != hi18n$lang.oi){
      log_warn("changing the target language of i18n..! (not re-using dict)")
      hi18n <- starti18n(lang.oi = lang.oi, addedDict = addedDict)
      hi18n <<- hi18n
    }
    
    if ("processName" %in% names(dfg)){
      if (is.ordered(dfg$processName)){
        log_info("translating ordered factor processNames. .")
        dfg$processName <- ht(as.character(dfg$processName)
                              , lang.oi = lang.oi
                              # , showSuffix = showSuffix
        )
        dfg$processName <- factor(dfg$processName
                                  , levels = unique(dfg$processName)
                                  , ordered = TRUE)
      } else {
        log_info("translating processNames. .")
        dfg$processName <- ht(dfg$processName
                              , lang.oi = lang.oi
                              # , showSuffix = showSuffix
        )
      }
    }
    if (!is.null(xlab)) xlab <- ht(xlab, lang.oi = lang.oi
                                   # , showSuffix = showSuffix
    )
    if (!is.null(ylab)) ylab <- ht(ylab, lang.oi = lang.oi
                                   # , showSuffix = showSuffix
    )
    if (!is.null(title)) title <- ht(title, lang.oi = lang.oi
                                     # , showSuffix = showSuffix
    )
    if (!is.null(subtitle)) subtitle <- ht(subtitle, lang.oi = lang.oi
                                           # , showSuffix = showSuffix
    )
    #zzz
  }
  
  ########################################################################
  ########################################################################
  
  # ..p <<- p
  # p <- geom_blank()
  # q <- geom_blank()
  # newplot <- identical(p, q, ignore.environment = TRUE)
  # newplot <- (p[1] == "new")
  # class(p)
  newplot <- !inherits(p, "gg")
  if (!newplot) newplot <- object.size2(p) < 150
  if (newplot) p <- ggplot2::ggplot()
  # ..p2 <<- p
  
  if (addgrad) {
    grad <- NULL
    grad[1] <- rgb(170/255, 225/255, 200/255)   #"#AAE1C8"
    grad[2] <- rgb(255/255, 255/255, 180/255)   #"#FFFFB4"
    grad[3] <- rgb(255/255, 185/255, 170/255)   #"#FFB9AA"
    ramp <- colorRamp(grad)   # c("#d0EEb0", "#FFEE30", "#EEb0b0")  green yellow, red
    RYG <- rgb( ramp(seq(0, 1, length = 5)), maxColorValue = 255)
    g <- grid::rasterGrob(RYG, width=unit(1,"npc"), height = grid::unit(1,"npc"), interpolate = TRUE)
    p <- p + annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  }
  
  if (data2layer0) {
    p <- ggplot2::ggplot(data=dfg, aes_string(x=as.name(xoi), y=as.name(yoi), label=label))
  }
  if(verbosity > 550) .p000 <<- p
  
  if ("right" %in% yaxis){
    yaxisNewlines <- c(0, 2)
  }
  
  if (!is.null(ylab)){
    #  add space before
    ylab <- paste0(rep("\n", yaxisNewlines[1]), ylab)
    #  add space after
    ylab <- paste0(ylab, rep("\n", yaxisNewlines[2]))
  }
  
  if (!is.null(xlab)){
    #  add space before
    xlab <- paste0(rep("\n", xaxisNewlines[1]), xlab)
    #  add space after
    xlab <- paste0(xlab, rep("\n", xaxisNewlines[2]))
  }
  
  # if (!"subtitle" %in% verbatim) if (!is.null(subtitle)) subtitle <- parse(text = subtitle)
  # if (!"title" %in% verbatim) if (!is.null(title)) title <- parse(text = title)
  # if (!"xlab" %in% verbatim) if (!is.null(xlab)) xlab <- parse(text = xlab)
  # if (!"ylab" %in% verbatim) if (!is.null(ylab)) ylab <- parse(text = ylab)
  if (isTRUE(grepl("expression", xlab))) xlab <- eval(parse(text=xlab)) 
  if (isTRUE(grepl("expression", ylab))) ylab <- eval(parse(text=ylab)) 
  if (isTRUE(grepl("expression", title))) title <- eval(parse(text=title)) 
  if (isTRUE(grepl("expression", subtitle))) subtitle <- eval(parse(text=subtitle)) 
  
  # cleanTitle <- function(title){
  #   title <- paste(title, collapse = "\n")
  #   if (length(title) == 0){
  #     title <- waiver()
  #   } else {
  #     if (nchar(title) == 0) title <- waiver()
  #   }
  #   title
  # }
  
  p <- p + 
    ggplot2::xlab(cleanChar(xlab, repl = NULL)) + 
    ggplot2::ylab(cleanChar(ylab, repl = NULL)) 
  
  # .titleDEF <<- title
  p <- p + labs(title = cleanTitle(title)
                , subtitle = cleanTitle(subtitle)
                , tag = cleanTitle(tag)
                , caption = cleanTitle(caption))  # used to be ggtitle()
  
  if(verbosity > 550) .p001 <<- p
  p <- p + themeAPH
  
  if(verbosity > 550){
    .p002 <<- p
  } 
  
  if (!is.null(abline[1])) {
    # a vector
    if (!is.list(abline)){
      p <- p + geom_abline(intercept = abline[1], slope = abline[2]
                           , color = ablinecolor
                           , size = lwdFit
                           , alpha = lineAlphaFit)
    } else {
      for ( ii in seq(length(abline))){
        p <- p + geom_abline(intercept = abline[[ii]][1], slope = abline[[ii]][2]
                             , color = ablinecolor
                             , size = lwdFit
                             , alpha = lineAlphaFit)
      }
    }
  }
  hline <- unlist(hline)
  if (!is.null(hline[1])) {
    # a vector
    for(i in seq(length(hline))) {
      p <- p + geom_hline(yintercept = hline[i]
                          , color = ablinecolor[min(length(ablinecolor),i)]
                          , size = lwdFit, alpha = lineAlphaFit
                          , linetype = lineTypeFit)
    }
  }
  vline <- unlist(vline)
  if (!is.null(vline[1])){
    # a vector
    for(i in seq(length(vline))) {
      .dfgdfg <<- dfg
      p <- p + geom_vline(xintercept = vline[i]
                          , color = ablinecolor[min(length(ablinecolor),i)]
                          , size = lwdFit, alpha = lineAlphaFit
                          , linetype = lineTypeFit)
    }
  }
  
  
  # lines (as path)
  lineFoi <- setdiff(foi, lineAcross)
  if (!length(lineFoi)) lineFoi <- NULL
  # log_info("foi {foi}, lineAcross {lineAcross}, lineFoi {lineFoi}")
  
  lineGroup <- setdiff(group, lineAcross)
  if (!length(lineGroup)) lineGroup <- NULL
  
  if (grepl("line", geom))  {
    
    if(is.null(lineColor)){
      if (!is.character(lineType)){
        # fixed lineType
        if (!is.character(lwd)){
          log_trace("fixed lwd {lineFoi}, {lineGroup}")
          # message("1270 1270 1270 1270 1270 1270 1270 1270 1270 ")
          # if (is.null(lineFoi)) lineFoi <- NA
          # if (is.null(lineGroup)) lineGroup <- NA
          # .zz <<- .data
          p <- p + geom_path(data = dfg
                             , aes_string(x = as.name(xoi)
                                          , y = as.name(yoi)
                                          , color = lineFoi
                                          , group = lineGroup
                             )
                             # , aes(x = .data[[xoi]]
                             #              , y = .data[[yoi]]
                             #              , color = .data[[lineFoi]]
                             #              , group = .data[[lineGroup]]
                             # )
                             , linetype = lineType
                             , linewidth = lwd
                             , alpha = lineAlpha)
        } else {
          log_debug("variable lwd")
          p <- p + geom_path(data=dfg
                             , aes_string(x=as.name(xoi), y=as.name(yoi)
                                          , color = lineFoi
                                          , linewidth = lwd
                                          , group = lineGroup)
                             , linetype = lineType
                             , alpha = lineAlpha)
        }
      } else {
        log_debug("variable lineType")
        p <- p + geom_path(data=dfg
                           , aes_string(x = as.name(xoi), y = as.name(yoi)
                                        , color = lineFoi
                                        , group = lineGroup
                                        , linetype = lineType
                           )
                           , linewidth = lwd
                           , alpha = lineAlpha)
      }
    } else {
      log_debug("fixed line color {lineGroup}")
      p <- p + geom_path(data=dfg
                         , aes_string(x=as.name(xoi), y=as.name(yoi)
                                      # , group = lineGroup
                         )
                         , linetype = lineType
                         , color = lineColor, linewidth = lwd, alpha = lineAlpha)
    }
  }
  
  
  
  if (verbosity > 550) {
    .p002b <<- p
  }
  
  

  if (grepl("ridge", geom)) {
    p <- p + geom_density_ridges(data = dfg
                        , aes_string(x = as.name(xoi)
                                     , y = as.name(yoi)
                                     , group = group, color =color)
                        )
    
  }
  
  if (grepl("point", geom)) {
    if (is.null(pointColor)){
      if (is.numeric(psize)){
        # .dfg1377 <<- dfg
        p <- p + geom_point(data = dfg
                            , aes_string(x = as.name(xoi), y = as.name(yoi)
                                         , color = foi, group = group)
                            , size = psize, alpha = pointAlpha)
      } else {
        # bubble??
        log_info("'bubble' 568 pggs")
        p <- p + geom_point(data = dfg
                            , aes_string(x = as.name(xoi), y = as.name(yoi)
                                         , color = foi, group = group
                                         , size = as.name(psize))
                            , alpha = pointAlpha)
      }
    } else {
      # normal
      # .dfg1393 <<- dfg
      p <- p + geom_point(data = dfg
                          , aes_string(x = as.name(xoi), y = as.name(yoi)
                                       , group = group)
                          , color = pointColor
                          , size = psize, alpha = pointAlpha)
    }
  }
  if (verbosity > 550) .p003 <<- p
  
  if (grepl("density",geom)){
    p <- p + geom_density(data = dfg
                          , aes_string(x = as.name(xoi), y = ..density..
                                       , fill = foi, group = group)
                          , alpha = alpha)
  }
  
  if (grepl("col", geom))   {
    p <- p + geom_col(data = dfg
                      , position = colPosition
                      , aes_string(x = as.name(xoi)
                                   , y = as.name(yoi)
                                   , group = group
                                   , fill = foi)
                      , alpha = alpha)
  }
  
  if (grepl("raster", geom)){
    p <- p + geom_raster(data = dfg, aes_string(x = as.name(xoi)
                                                , y = as.name(yoi)
                                                , fill = zoi)
                         , interpolate = interpolate)
  }
  
  
  if (grepl("boxplot", geom)){
    p <- p + geom_boxplot(data = dfg
                          , aes_string(x = as.name(xoi)
                                       , y = as.name(yoi)
                                       , fill = foi
                                       , group = group),
                          outlier.colour = NULL,
                          outlier.color = NULL,
                          outlier.fill = NULL,
                          outlier.shape = 19,
                          outlier.size = 1.5,
                          outlier.stroke = 0.5,
                          outlier.alpha = NULL,
                          notch = FALSE,
                          notchwidth = 0.5,
                          varwidth = FALSE,
                          na.rm = FALSE,
                          orientation = NA,
                          show.legend = NA,
                          inherit.aes = TRUE    ) 
  }
  
  
  if (grepl("violin", geom)){
    p <- p + geom_violin(data = dfg
                         , aes_string(x = as.name(xoi)
                                      , y = as.name(yoi)
                                      , fill = foi
                                      , group = group)
                         , alpha = violinAlpha
                         , trim = violinTrim
                         , adjust = violinAdjust
                         , scale = violinScale
                         , draw_quantiles = violinDraw_quantiles
                         , color = violinColor
    ) 
    if (length(violinProbs)){
      median.quartile <- function(x){
        out <- quantile(x, probs = violinProbs)
        names(out) <- c("ymin", "y", "ymax")
        return(out) 
      }
      p <- p + stat_summary(data = dfg
                            , aes_string(x = as.name(xoi)
                                         , y = as.name(yoi)
                                         , fill = foi
                                         , group = group)
                            , alpha = violinAlpha
                            , fun = median.quartile
                            , geom = 'point')
    }
    if (jitterWidth > 0 | jitterHeight > 0){
      jitterSeed <- 1234
      set.seed(jitterSeed)
      p <- p + geom_jitter(data = dfg
                           # , aes(x = get(xoi), y = get(yoi))
                           , aes_string(x = as.name(xoi)
                                        , y = as.name(yoi)
                                        , fill = foi
                                        , group = group
                           )
                           , color = jitterColor
                           , width = jitterWidth
                           , height = jitterHeight
      )
    }
  }
  
  if (grepl("ribbon", geom)){
    log_trace("for ribbon: xoi {xoi}")
    log_debug("for ribbon: ymin {ymin}, color: {ribbonColor}")
    .dfgRibbon <<- dfgRibbon
    # .dfgRibbon <<- copy(dfgRibbon)
    if (ribbonColor %in% names(dfgRibbon)){
      str(ribbonColor)
      log_debug("using ribboncolor {ribboncolor} column in data dfgRibbon")
      print(glue("using ribboncolor {ribboncolor} column in data dfgRibbon"))
      p <- p + geom_ribbon(data = dfgRibbon
                           , aes_string(x = as.name(xoi)
                                        , ymin = as.name(ymin)
                                        , ymax = as.name(ymax)
                                        , group = as.name(foi) # lineGroup?
                                        , fill = as.name(ribbonColor)
                                        # 
                           )
                           , alpha = ribbonAlpha)
    } else {
      # dfgRibbon <- dfgRibbon[, ]
      message("fixed ribboncolor")
      p <- p + geom_ribbon(data = dfgRibbon
                           , aes_string(x = as.name(xoi)
                                        , ymin = as.name(ymin)
                                        , ymax = as.name(ymax)
                                        , group = as.name(foi) # lineGroup?
                                        # , fill = foi
                           )
                           , fill = ribbonColor
                           , alpha = ribbonAlpha)
    }
  }
  
  if (grepl("libbon", geom)){
    log_trace("for libbon: xoi {xoi}")
    log_trace("for libbon: ymin {ymin}")
    # .dfgRibbon <<- copy(dfgRibbon)
    if (!is.null(lineGroup))
      if (!lineGroup %in% names(dfgRibbon)) lineGroup <- NULL
    p <- p + geom_path(data = dfgRibbon
                       , aes_string(x = as.name(xoi)
                                    , y = as.name(ymin)
                                    , group = lineGroup
                       )
                       , linetype = libbonLineType
                       , size = libbonSize
                       , alpha = libbonAlpha
                       , color = libbonMinColor
    )
    p <- p + geom_path(data = dfgRibbon
                       , aes_string(x = as.name(xoi)
                                    , y = as.name(ymax)
                                    , group = lineGroup
                       )
                       , linetype = libbonLineType
                       , size = libbonSize
                       , alpha = libbonAlpha
                       , color = libbonMaxColor
    )
  }
  
  if (grepl("pibbon", geom)){
    log_trace("for pibbon: xoi {xoi}")
    log_trace("for pibbon: ymin {ymin}")
    # .dfgRibbon <<- copy(dfgRibbon)
    p <- p + geom_point(data = dfgRibbon
                        , aes_string(x = as.name(xoi)
                                     , y = as.name(ymin)
                        )
                        , size = libbonSize
                        , alpha = libbonAlpha
                        , color = libbonMinColor
    )
    p <- p + geom_point(data = dfgRibbon
                        , aes_string(x = as.name(xoi)
                                     , y = as.name(ymax)
                        )
                        , size = libbonSize
                        , alpha = libbonAlpha
                        , color = libbonMaxColor
    )
  }
  
  if (grepl("medal", geom)){
    log_trace("for medal-ribbon: xoi {xoi}")
    log_trace("for medal-ribbon: ymin {ymin}")
    p <- p + geom_ribbon(data = dfg
                         , aes_string(x = as.name(xoi)
                                      , ymin = ymin
                                      , ymax = ymax
                                      , fill = ribbonColor
                                      , alpha = ribbonAlpha
                         )
    )
    # pp <<- p
  }
  
  
  if (grepl("errorhbar", geom)){
    log_trace("for errorbarh: yoi {yoi}")
    log_trace("for errorbarh: xmin {xmin}")
    p <- p + geom_errorbarh(data = dfg
                            , aes_string(y = as.name(yoi)
                                         , xmin = xmin, xmax = xmax
                                         , color = foi, group = group)
                            , height = errorWidth
                            , linetype = lineTypeEB
                            , size = lwdEB, alpha = lineAlphaEB
    )
  }
  if (grepl("errorbar", geom)){
    log_trace("for errorbar: xoi {xoi}")
    log_trace("for errorbar: ymin {ymin}")
    p <- p + geom_errorbar(data = dfg
                           , aes_string(x=as.name(xoi)
                                        , ymin=ymin, ymax=ymax
                                        , color=foi, group=group)
                           , width = errorWidth
                           , linetype = lineTypeEB
                           , size=lwdEB, alpha=lineAlphaEB
    )
  }
  
  
  if (grepl("tile", geom)){
    # log_trace("for tile: foi {foi}")
    # xx <- table(dfg[, foi], useNA = "always")
    # if (length(xx) > 21){
    #   xx <- xx[1:length(xx)]
    # }
    # print(xx)
    p <- p + geom_tile(data = dfg
                       , aes_string(x = as.name(xoi)
                                    , y = as.name(yoi)
                                    , fill = as.name(zoi)))
    # p <- p + scale_fill_manual(values=c("green", "yellow", "red"), 
    #                     breaks = c("green", "yellow", "red"))
    if(verbosity > 550) .pTile <<- p
  }
  if (verbosity > 550) .p004 <<- p
  
  # suppressWarnings({
  {
    # if (!is.null(facet_w)) p <- p + facet_wrap(facet_w, scales="free_y")
    if (free_y) facetscales = "free_y"
    if (free_x) facetscales = "free_x"
    if (free_y & free_x) facetscales = "free"
  
    if (!is.null(facet_w)) {
      p <- p + facet_wrap(
        as.formula(paste("~", as.name(facet_w)))
        , drop = TRUE 
        , nrow = facetrows
        , ncol = facetcols
        , scales = facetscales
        , labeller = facetlabeller
      )
    }
    
    #   ggforce::facet_wrap_paginate(~cut:clarity, ncol = 3, nrow = 3, page = 1)
    
    if (!is.null(facet_g)) {
      p <- p + facet_grid(as.formula(facet_g)
                          , drop = TRUE 
                          # , nrow = facetrows, ncol = facetcols
                          , scales = facetscales
                          # , labeller = facetlabeller
                          , margins = FALSE
      )
      .p1633 <<- p
    }
    if (!is.null(facet_w) | (!is.null(facet_g))) {
      p <- p + ggplot2::theme(strip.text=ggplot2::element_text(size=stripSize))
      # p <- p + ggplot2::theme(strip.background = ggplot2::element_rect(
      #   color="black", fill="#ffffff", size=14, linetype="solid"))
    }
    # to remove striprectangles: p <- p + ggplot2::theme(strip.background = ggplot2::element_blank())
    
    
    # hiyois = c("SGR", "temperature")
    if (length(hiyois)){
      more <- c(more, 'scale_fill_identity()')
      variable.name <- aphVariables(as.data.table(dfg))
      backgr <- unique(as.data.table(dfg)[, ..variable.name])
      backgr[, bcol := ifelse(get(variable.name) %in% hiyois, hiyoisColor, NA)]
      p <- p + geom_rect(data = backgr,
                  aes(fill = bcol),
                  xmin = -Inf, xmax = Inf, 
                  ymin = -Inf, ymax = Inf,
                  alpha = hiyoisAlpha,  # Transparency [dimensionless]
                  inherit.aes = FALSE )
    }
    if (length(hiyois)){
      p <- p + scale_fill_identity()
    }
    
    
    if (!legend %in% c("auto")) {
      p <- p + ggplot2::theme(legend.position = legend
                              , legend.text = ggplot2::element_text(size=legendSize))
    }
    
    if (legendTitle == "none") {
      p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
    } else {
      p <- p + ggplot2::theme(legend.title = ggplot2::element_text(legendTitle))
    }
    
    if (legendTitle != "auto") {
      if (continuousFoi){
        p <- p + scale_fill_continuous(guide = guide_legend(title = legendTitle))
      } else {
        p <- p + scale_fill_discrete(guide = guide_legend(title = legendTitle))
      }
      # .ppp00 <<- p
      # p <- p + guides(fill=guide_legend(title = legendTitle))
    }
    
    
    xvjust=.5; xhjust=1
    yvjust=.5; yhjust=1
    if (xangle != 90) { xvjust=1; xhjust=1}
    if (xangle == 0 ) { xvjust=1; xhjust=0.5}
    if (xangle < 0 ) { xvjust=-1; xhjust=0.5}
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(
      size=xsize, angle=xangle, vjust=xvjust, hjust=xhjust))
    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(
      size=ysize, angle=yangle, vjust=yvjust, hjust=yhjust))
    if(xaxis == "none")    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank())
    if(yaxis == "none")    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())
    
    # set xsc ysc from prior ggplot object 'pxysc'
    if ("ggplot" %in% class(pxysc)){
      ww <- ggplot_build(pxysc)
      # if (ysc[1] == "extract") {
      ysc = ww$layout$panel_params[[1]]$y.range
      # }
      log_trace("ysc now: ",paste(ysc, collapse=" "))
      # if (as.character(xsc[1]) == "extract") {
      xsc = ww$layout$panel_params[[1]]$x.range
      # if (xshift != 0) xshift = xshift*diff(xsc)
      # }
      log_trace("xsc now: ",paste(xsc, collapse=" "))
      # log_trace("xshift now: ",paste(xshift))
    }
    
    
    # xsc redefined by xsc_q (Quantiles of the range)
    if(!is.null(xsc_q)){
      if(xsc_q[1] >= 0) xsc[1] = quantile(unlist(dfg[,xoi[1]]), max(0,min(1,xsc_q[1])), na.rm=TRUE)
      if(xsc_q[2] <= 1) xsc[2] = quantile(unlist(dfg[,xoi[1]]), max(0,min(1,xsc_q[2])), na.rm=TRUE)
      if(is.na(xsc[1])) xsc[1] <- min(dfg[,xoi], na.rm=TRUE)
      if(is.na(xsc[2])) xsc[2] <- max(dfg[,xoi], na.rm=TRUE)
      log_trace("xsc = ", paste(xsc, collapse=", "))
    }
    if(!is.null(ysc_q)){
      if(ysc_q[1] >= 0) ysc[1] = quantile(unlist(dfg[,yoi[1]]), min(1,ysc_q[1]), na.rm=TRUE)
      if(ysc_q[2] <= 1) ysc[2] = quantile(unlist(dfg[,yoi[1]]), max(0,ysc_q[2]), na.rm=TRUE)
      if(is.na(ysc[1])) ysc[1] <- min(dfg[,yoi], na.rm=TRUE)
      if(is.na(ysc[2])) ysc[2] <- max(dfg[,yoi], na.rm=TRUE)
      log_trace("ysc = ", paste(ysc, collapse=", "))
    }
    
    if (xsc_ysc == 1){
      if (is.null(xsc)) xsc <- hrange(dfg[, xoi])
      if (is.null(ysc)) ysc <- hrange(dfg[, yoi])
      mi <- hmin(xsc[1], ysc[1])
      if (is.infinite(mi)) mi <- NA
      ma <- hmax(xsc[2], ysc[2])
      if (is.infinite(ma)) ma <- NA
      log_info("made common xsc = ysc: {mi}, {ma}")
      xsc[1] <- ysc[1] <- mi
      xsc[2] <- ysc[2] <- ma
    }
    
    if (aspect.ratio != "auto") {
      p <- p + coord_fixed(ratio=aspect.ratio, xlim=xsc, ylim=ysc)
    } else {
      p <- p + coord_cartesian(xlim=xsc, ylim=ysc)
    }
    
    if (yextra[1] != "auto") {
      if (verbosity > 550) dfg.bar <<- dfg
      yr = range(dfg[,yoi], na.rm=T)
      # log_trace(yr)
      yr[1] = yr[1] - diff(yr)*yextra[1]
      yr[2] = yr[2] + diff(yr)*yextra[2]
      # log_trace(yr)
      p <- p + expand_limits(y = yr)
    }
    if (xextra[1] != "auto") {
      xr = range(dfg[, xoi], na.rm=T)
      # log_trace("xextra")
      # log_trace(paste(xr))
      xr[1] = xr[1] - diff(xr)*xextra[1]
      xr[2] = xr[2] + diff(xr)*xextra[2]
      .xr <<- xr
      # log_trace(paste(xr))
      p <- p + expand_limits(x = xr)
    }
    
    
    if (newplot & alwaysnewxaxes){
      log_trace("setting axes------------------------------------------------")
      log_trace("xoi: {xoi}")
      xx <- class(dfg[, xoi])
      log_trace("xoi class: {xx}")
      ################################################## XXaxis
      if ("POSIXct" %in% class(dfg[, xoi]) ){
        log_trace("POSIXct breaks")
        if (!is.null(datebreaks)){
          #TODO 
          # str(datebreaks)
          # str(date_minor_breaks)
          # str(dateformat)
          if (inherits(datebreaks, "character")){
            p <- p + scale_x_datetime(date_breaks = datebreaks
                                      # , date_minor_breaks = date_minor_breaks # date_minor_breaks_tz(hinterval=minorbreakInterval, tz=tz)
                                      , date_labels = dateformat
                                      # labels = date_format_tz(dateformat, tz=tz)  #OLD deprec
            )#, expand=dateexpand) #limits=xsc,
          } else {
            p <- p + scale_x_datetime(breaks  = datebreaks,
                                      date_labels = dateformat
            )
          }
        }
      }
      if ("Date" %in% class(dfg[, xoi]) ){
        log_trace("Date breaks")
        if (!is.null(datebreaks)){
          if (inherits(datebreaks, "character")){
            p <- p + scale_x_date(date_breaks  = datebreaks,
                                  # minor_breaks = date_minor_breaks_tz(hinterval=minorbreakInterval, tz=tz),
                                  date_labels = dateformat
            )#, expand=dateexpand) #limits=xsc,
          } else {
            p <- p + scale_x_date(breaks  = datebreaks,
                                  date_labels = dateformat)
          }
        }
      }
      if (is.numeric(dfg[,xoi]) & geom != "density"){
        log_trace("setting scale_x_continuous-------------------------------")
        if (xtics == "months") {
          if (is.null(xsc)) xsc=c(1,12)
          p <- p + scale_x_continuous( breaks=seq(12), labels=monthnames, limits=xsc, expand=expandX)
        } else {
          if (xtics == "maanden") {
            if (is.null(xsc)) xsc=c(1,12)
            maanden1 <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
            maanden <- maanden3 <- c("Jan", "Feb", "Mar", "Apr", "Mei", "Juni", "Juli", "Aug", "Sept", "Okt", "Nov", "Dec")
            p <- p + scale_x_continuous( breaks=seq(12), labels=maanden, limits=xsc, expand=expandX)
          } else {
            if (xtics != "auto") {
              p <- p + scale_x_continuous(breaks=(seq(5000)-2500)*xtics, limits=xsc, expand=expandX)
            } else{
              p <- p + scale_x_continuous(expand=expandX)
            }
          }
        }
      }
      
      ################################################## YYaxis
      if (ytics == "sensory") {
        if (is.null(ysc)) ysc=c(1,5)
        sensory <- rep(" ", 5)
        sensation <- c("sweet.intensity" = "sweet"
                       , "firmness" = "firm"
                       , "juiciness" = "juicy"
                       , "ripeness" = "ripe"
                       , "purchase.intent"="attractive to buy")
        
        sensory[1] <- paste0("Not ",sensation[yoi]," at all (1)")
        sensory[5] <- paste0("Very ",sensation[yoi], " (5)")
        
        p <- p + scale_y_continuous(breaks = seq(5), labels = sensory, oob = rescale_none
                                    , expand = expandY
                                    , position = yaxisPosition
        ) #limits = ysc,
      } else {
        if (ytics == "hedonic") {
          if (is.null(ysc)) ysc = c(1,9)
          #       hedonic = rev(c("Like Extremely", "Like very much", "Like Moderately", "Like Slightly", "Neither Like nor Dislike",
          #                       "Dislike Slightly", "Dislike Moderately", "Dislike Very Much", "Dislike Extremely"))
          hedonic = rev(c("Like Extremely (9)", "Like very much (8)", "Like Moderately (7)", "Like Slightly (6)", "Neither Like nor Dislike (5)",
                          "Dislike Slightly (4)", "Dislike Moderately (3)", "Dislike Very Much (2)", "Dislike Extremely (1)"))
          p <- p + scale_y_continuous(breaks = seq(9), labels = hedonic, oob = rescale_none
                                      , expand = expandY
                                      , position = yaxisPosition
          ) #limits=ysc,
        } else {
          if (ytics != "auto") {
            log_trace("scale_y_continuous--------------------------------yyyyyyyyyyyyyyyyyyyyy")
            p <- p + scale_y_continuous(breaks = (yticsShift + seq(500)-250)*ytics
                                        , expand = expandY
                                        , position  =  yaxisPosition
            )#, limits=ysc)
          } else {
            # .yoi <<- yoi
            if (all(yoi %in% names(dfg))){
              if (is.numeric(dfg[, yoi])){
                p <- p + scale_y_continuous(expand = expandY
                                            , position = yaxisPosition
                )#, limits=ysc)
              }}
          }
        }
      }
    } ############## all axis formatting
    
    
    ################################################## ELLIPSE ######################
    el.alpha = el.alpha[!el.alpha ==1]
    for (ee in el.alpha){
      ee = as.numeric(ee)
      ellipsedistr = "norm"
      el.geom = ifelse(el.fill, "polygon", "path")
      if (ellipsedistr == "norm"){
        p <- p + stat_ellipse(data=dfg, aes_string(x=as.name(xoi), y=as.name(yoi), group=group, color=foi)
                              , geom=el.geom, size=el.lwd,
                              na.rm=T, segments = 101, type="norm", level=1-ee
                              , alpha=min(1, 0.2+ee))
      } else{  #e.g. t-distribution
        p <- p + stat_ellipse(data=dfg, aes_string(x=as.name(xoi), y=as.name(yoi), group=group, color=foi)
                              , geom=el.geom,
                              na.rm=T, segments = 101, type="t",    level=1-ee
                              , alpha=min(1, 0.2+ee))
      }
    }
    
    ################################################## MODEL FIT ######################
    if (poly == 0) {
      ci.alpha <- NULL
    }
    for (w in ci.alpha){
      w <- as.numeric(w)
      woi <- "smoothweight"
      wasDate <- FALSE
      if (inherits(dfg[, xoi], "Date")){
        wasDate <- TRUE
        dfg[, xoi] <- as.POSIXct(dfg[, xoi])
      }
      if (inherits(dfg[, xoi], "POSIXct")){
        woi <- NULL
      } else {
        .dfg0101 <<- dfg
        xoi.mean = mean(dfg[, xoi], na.rm =TRUE)
        dfg[, woi] = abs( dfg[, xoi] / xoi.mean - 1 )^weight.expo  #FIXME wrong  (ok when dt4fit = 0)
      }
      if (wasDate){
        dfg[, xoi] <- as.Date(dfg[, xoi])
      }
      if (method == "lm"){
        if (is.null(fitColor)){
          p <- p + stat_smooth(
            data = dfg
            , aes_string(x = as.name(xoi)
                         , y = as.name(yoi)
                         , group = group
                         , color = foi
                         , fill = foi)
            #, weight=woi
            , level = 1-w
            , geom = statgeom
            , method = method
            , formula = formula
            , alpha = min(1,0.2 + w)
            , size = lwdFit
            , na.rm = TRUE
            , show.legend = ("fit" %in% doShow_guide))  # , interval="prediction"
        } else {
          p <- p + stat_smooth(data = dfg
                               , aes_string(x=as.name(xoi)
                                            , y=as.name(yoi)
                                            , group=group
                               ) #, weight=woi
                               , level = 1-w
                               , geom=statgeom
                               , method=method
                               , formula=formula
                               , color = fitColor
                               , fill = fitColor
                               , alpha = min(1,0.2+w), size=lwdFit, na.rm=TRUE
                               , show.legend = ("fit" %in% doShow_guide))  # , interval="prediction"
        }
        # log_trace("test method poly2 etc", formula, method, statgeom)
      } else{  #e.g. loess method
        p <- p + stat_smooth(
          data=dfg
          , aes_string(x=as.name(xoi)
                       , y=as.name(yoi)
                       , group=group
                       , color=foi
                       , fill=foi
                       , weight=woi
          )
          , level=1-w, geom=statgeom, method=method, alpha=min(1,0.2+w), size=lwdFit,
          na.rm=TRUE
          , show.legend = ("fit" %in% doShow_guide))
      }
    }
    if (annoFit & (method == "lm") & (poly < 2)){
      formula = "y~x"
      log_trace("annoFit formula ", formula)
      log_trace("xoiFit  ", xoiFit)
      # log_trace(paste(names(dfg),sep="|"))
      dfg2 = dfg
      dfg2$y = dfg2[,yoi]
      dfg2$x = dfg2[,xoiFit]
      .dfgFitted <<- dfg2
      fit <- lm(formula, dfg2)
      text2show <- lm_eqn(fit, digits=2)
      # .text2show <<- text2show
      if (0 < sign(coef(fit)[2])){
        my_grob = grid::grobTree(grid::textGrob(
          text2show, x=0.02,  y=0.98, hjust=0
          , gp=grid::gpar(col="black", fontsize=12, fontface="bold")))
      } else{
        my_grob = grid::grobTree(grid::textGrob(
          text2show, x=0.98,  y=0.98, hjust=1
          , gp=grid::gpar(col="black", fontsize=12, fontface="bold")))
      }
      # .my_grob <<- my_grob
      p <- p + annotation_custom(my_grob)
    }
    
    # hash
    if (hash[1] != "none") {
      if (hash[1] == "auto") {
        if (!exists("script")){
          script = "u"
        }
      }
      hash <- paste(script, htimestamp())
      hash = grid::grobTree(grid::textGrob(
        hash
        , x=0.98,  y=0.02, hjust=1
        , gp = grid::gpar(col="black", fontsize=5)))
      p <- p + annotation_custom(hash)
    }
    
    {
      if(length(pal.oi)){
        # pal.std <- c("black", "red", "green3", "blue", "cyan", "magenta", "yellow", "gray")
        pal.std = c("black", "red", "green", "blue", "magenta", "orange", "cyan", "violet", "darkred", "darkgreen", "darkblue", "gray")
        pal.skip <- pal.std[-1]
        # bayer <- modelReduction::bayerPalette()
        # if (pal.oi != "pal.std"){
        # log_trace(pal.oi)
        # cp <- ggthemes::canva_palettes
        # if (pal.oi[1] %in% names(cp)) {
        #   palette.oi <- cp[[pal.oi]]
        # } else {
        if (!exists(pal.oi, mode="character")) {
          log_error("pggs| palette not found: {pal.oi}")
        } else {
          palette.oi <- base::get(pal.oi)
        }
      }
      if (doublePalette){
        # palette.oi <- bayer
        nnn <- length(palette.oi)
        # ddd <- sort(c(seq(0, 1, length = nnn), seq(0, 1, , length = nnn) - 0.03))[-(1:2)]
        # palette.oi <- rgb(colorRamp(palette.oi)(ddd), max = 255)
        ddd <- rep(seq(0, 1, length = nnn), each = 2)
        palColors <- colorRamp(palette.oi)(ddd)
        alphas <- rep(c(255, 31), nnn)
        palette.oi <- rgb(palColors, alpha = alphas, max = 255)
        palette.oi
      }
      # }
      palette.oi = rep(unname(palette.oi), 500)
      if (grepl("raster", geom)){
        p <- p + scale_fill_manual(values = palette.oi)
      }
      p <- p + scale_color_manual(values = palette.oi)
    }
  }
  if(verbosity > 550) .p005 <<- p
  # p
  # }) # FIXME
  
  
  for (voi in noLegendItem){
    p <- p + guides(as.list(setNames("none", voi)))
  }
  p <- p + guides(variable = "none")
  
  if(verbosity > 550) .p006 <<- p
  
  # if (!is.null(planting_week)){
  #   wk40brks <- c(planting_week:52, 1:(planting_week-1))
  #   wk40vis <- c(seq(planting_week, 52, 2), 1, seq(2, planting_week, 2))
  #   wk40labs <- wk40vis
  #   # message(wk40vis)
  #   # print(wk40vis)
  #   # wk40labs <- wk40brks
  #   # wk40labs[!wk40labs %in% wk40vis] <- ""
  #   p <- p + scale_x_continuous(breaks = wk40vis, labels = wk40labs)#, minor_breaks = NULL)
  #   # p <- p + scale_x_discrete(breaks = wk40vis, labels = wk40labs)
  # }
  
  p <- paddp(p, more)
  if(verbosity > 550) .p007 <<- p
  
  
  if (logx) p <- p + scale_x_log10()
  if (logy) p <- p + scale_y_log10()
  if (flip) p <- p + coord_flip()
  
  if (!labelorig[1] == "none") {
    
    if (flip) {
      labelXjust.tmp = labelXjust
      labelXjust = labelYjust
      labelYjust = labelXjust.tmp
    }
    # if (!is.null(labelDigits)){
    #   log_info("rounding labels")
    #   dfg[, label] <- round(dfg[, label], digits = labelDigits)
    # }
    xoi2 = ifelse(is.null(labelX), xoi, labelX)
    yoi2 = ifelse(is.null(labelY), yoi, labelY)
    
    # .p.beforeText <<- p
    # .dfg.beforeText <<- copy(as.data.table(dfg))
    # .labelXjust0 <<- labelXjust
    # .labelColor <<- labelColor
    
    if (doLabelVarColor) {
      labelVarColor <- foi
    } else {
      labelVarColor <- "varColor"
    }
    
    # .dfg4label <<- dfg
    dfg$label <- substr(dfg$label, 1, pmin(nchar(as.character(dfg$label)), labelnchars))
    dfg$label <- labelMod(dfg$label)
    p <- addText(p
                 , dfg
                 , xoi2
                 , yoi2
                 , labelYmid = labelYmid
                 , laboi = label
                 , color = labelColor
                 , varColor = labelVarColor
                 , size = labelSize
                 , fontface = 3
                 , angle = labelAngle, hjust = labelXjust, vjust = labelYjust
                 , labelRepel = labelRepel
                 , max.overlaps = max.overlaps)
  }
  if(verbosity > 550) .p008 <<- p
  
  if (length(hiyois)){
    p <- p + scale_fill_identity()
  }
  
  
  if (is.null(chunkId) & g.reqCnk) g.addRmd <- FALSE
  
  #   if (doplot)
  if (slideText == "rows") slideText = paste0(dim(dfg)[1], " rows")
  
  if (allowppt & g.addRmd) {
    # log_trace("in pggs, on to vrmd {paste(g.addppt , addppt , g.addRmd)}")
    if (g.addppt | addppt | g.addRmd){
      # check if each ... is in formals(vrmd)
      FUN <- "vrmd"
      if (!g.addRmd & g.addppt) FUN <- "vppt"
      # dots <- list(...)
      # only extract dots for vrmd (or vppt)
      # dots4Here <- intersect(names(dots), names(formals(FUN)))
      vrmdArgList <- list(do = "add"
                          , p = p
                          , chunkId = chunkId
                          , ggwidth = ggwidth
                          , ggheight = ggheight
                          , chartClassAdded = chartClassAdded
                          , doTabbed = doTabbed
                          , dpi = dpi
                          , childWidth2 = childWidth2
      )
      
      if (FUN == "vppt"){
        vrmdArgList$title = slideTitle
        vrmdArgList$text  = slideText
        FUN <- vppt
      } else {
        FUN <- vrmdApp
      }
      # vrmdArgList <- c(vrmdArgList, dots[dots4Here])
      # .vrmdArgList <<- vrmdArgList
      log_trace("in pggs, sending to {FUN} local g.addRmd = {g.addRmd}")
      # hstr(vrmdArgList)
      # FUN <- get(FUN)
      resvrmd <<- do.call(FUN, vrmdArgList) #aspect.ratio=aspect.ratio,
    }
  }
  
  if(!is.null(png)){
  pngpath <- suppressMessages(suppressWarnings({
    dev2png(p = p
            , ffpng.out = paste0(pngdir, make.names3(ASCIIfy(png)), ".png")
            , ggwidth = 18, ggheight = 10, dpi = 150)
  }))
  log_info(paste0("chart saved as ", pngpath))
  }
  
  
  if (exists("p")) if ("ggplot" %in% class(p)) p$data = dfg
  if (ggplotly | g.ggly){
    p <- plotly::ggplotly(p)
  }
  if (!g.doplot) doplot <- FALSE
  if (doplot) {
    print(p)
  }
  
  if (docuCall){
    attr(p, "SYSCALL") <- sys.call(which=0)
    if(verbosity >= 400) .last.pggs <<- match.call()
    attr(p, "CALL") <- match.call()
    # attr(p, "USEDFUN") <- sys.function(sys.parent())  # 7Mb!
  }
  if (sleep > 0){
    log_trace("sleeping: ", sleep)
    Sys.sleep(sleep)
  }
  return(p)
}



# added by autoDocument, row.1346 | Tue Mar 28 16:32:49 2017
#' paddp
#'
#' @author Hans.Schepers@@gmail.com
#' @export
paddp <- function(p, more = "none"){
  if (!length(more)) return(p)
  if (more[1] == "none") return(p)
  for (imore in more){
    p <- eval(parse(text = paste("p + ", imore)))
  }
  return(p)
}



# added by autoDocument, row.1359 | Tue Mar 28 16:32:49 2017
#' addText
#'
#' @author Hans.Schepers@@gmail.com
#' @export
addText <- function(p
                    , dfg
                    , xoi = "dateTime"
                    , yoi = "value"
                    , laboi = "label"
                    , color = "grey"
                    , labelRepel = 0
                    , varColor = "varColor"
                    , max.overlaps = 20
                    , size = 12
                    , fontface = 3, angle = 0, hjust = 0.5, vjust = 0.5
                    , labelXmid = 1, labelYmid = 1){
  dfg <- as.data.frame(dfg)
  # if ("color" %in% names(dfg)) varColor = "color"
  if (!"angle" %in% names(dfg)) dfg$angle = angle
  if (is.numeric(hjust)) { dfg$hjust = hjust } else { dfg$hjust = dfg[,hjust] }
  if (is.numeric(vjust)) { dfg$vjust = vjust } else { dfg$vjust = dfg[,vjust] }
  dfg[,laboi] = as.character(dfg[,laboi])
  if (is.numeric(xoi)) {dfg[,"xoi"]=xoi ; xoi="xoi"}
  if (is.numeric(yoi)) {dfg[,"yoi"]=yoi ; yoi="yoi"}
  if (is.numeric(dfg[,xoi])) dfg[,xoi] = dfg[,xoi]*labelXmid
  if (is.numeric(dfg[,yoi])) dfg[,yoi] = dfg[,yoi]*labelYmid
  #   log_trace("addtext:")
  #   print(dim(dfg))
  dfg.addtext <<- dfg
  if (!varColor %in% names(dfg)){
    # log_trace("adding 'varColor' to dfg")
    # dfg[, "varColor"] <- color
    # getOption("ggrepel.max.overlaps", default = 20)
    if ((labelRepel == 1) & (.Platform$OS.type == "windows")){
      p <- p + ggrepel::geom_text_repel(
        data = dfg
        , ggplot2::aes_string(x = as.name(xoi)
                              , y = as.name(yoi)
                              , label = laboi
        )
        , color = color
        , size = size
        , fontface = fontface
        , max.overlaps = max.overlaps)
    }
    if ((labelRepel == 2) & (.Platform$OS.type == "windows")){
      p <- p + ggrepel::geom_label_repel(
        data = dfg
        , ggplot2::aes_string(x = as.name(xoi)
                              , y = as.name(yoi)
                              , label = laboi
        )
        , color = color
        , size = size
        , fontface = fontface
        , max.overlaps = max.overlaps)
    }
    if ((labelRepel == 0) | (.Platform$OS.type != "windows")){
      p <- p + ggplot2::geom_text(
        data = dfg
        , ggplot2::aes_string(x = as.name(xoi)
                              , y = as.name(yoi)
                              , label = laboi
                              , hjust = "hjust"
                              , vjust = "vjust"
                              , angle = "angle"
        )
        , color = color
        , size = size
        , fontface = fontface)
    }
    
    
  } else {
    # message("addText|")
    # str(size)
    if ((labelRepel == 1) & (.Platform$OS.type == "windows")){
      p <- p + ggrepel::geom_text_repel(
        data = dfg
        , ggplot2::aes_string(x = as.name(xoi)
                              , y = as.name(yoi)
                              , label = laboi
                              , color = varColor)
        , size = size
        , fontface = fontface
        , max.overlaps = max.overlaps)
    }
    if ((labelRepel == 2) & (.Platform$OS.type == "windows")){
      p <- p + ggrepel::geom_label_repel(
        data = dfg
        , ggplot2::aes_string(x = as.name(xoi)
                              , y = as.name(yoi)
                              , label = laboi
                              , color = varColor)
        , size = size
        , fontface = fontface
        , max.overlaps = max.overlaps)
    }
    if ((labelRepel == 0) | (.Platform$OS.type != "windows")){
      p <- p + ggplot2::geom_text(
        data = dfg
        , ggplot2::aes_string(x = as.name(xoi)
                              , y = as.name(yoi)
                              , label = laboi
                              , hjust = "hjust"
                              , vjust = "vjust"
                              , color = varColor
                              , angle = "angle"
        )
        # , color = color
        , size = size
        , fontface = fontface)
    }
  }
  return(p)
}


#' addAnnotation
#'
#' @author Hans.Schepers@@gmail.com
#' @export
addAnnotation <- function(p, geom = "text", x = 60, y = 95, label = "Consumer Liking?",
                          color = "black", size = 12, fontface = 3, angle = 0, hjust = 0, vjust = 0, parse = TRUE){
  p <- p + ggplot2::annotate(geom = geom, x = x, y = y, label = label, color = color, size = size, fontface = fontface,
                             angle = angle, hjust = hjust, vjust = vjust, parse = parse)
  return(p)
}


#' addRect
#'
#' @author Hans.Schepers@@gmail.com
#' @export
addRect <- function(p, xmin = 55, xmax = 65, ymin = 70, ymax = 100, fill = "grey", alpha = 0.15){
  #   p <- p + geom_rect(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = "grey", alpha = 0.05)
  p <- p + ggplot2::annotate(geom = "rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, alpha = alpha)
  return(p)
}


#' createPolygon
#' 
#' @export
createPolygon <- function(DT, minVals, maxVals){
  plot(DT$dateTime,DT$value,
       xlab = 'date', ylab = 'daily growth (gram)', ylim=c(0,max(maxVals)), type='l')
  selTimes <- DT$dateTime
  polygon(c(selTimes, rev(selTimes)), c(minVals, rev(maxVals)),    
          col=rgb(1, 0, 0,0.5), border=NA)
}


#' lm_eqn
#'
#' @author Hans.Schepers@@gmail.com
#' @export
lm_eqn <- function(m, digits=2, adj=FALSE){
  if (class(m) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(m)$fstatistic
  pv <- pf(f[1],f[2],f[3],lower.tail=F)
  pv = format(pv, digits=digits)
  
  adjp=""; if(adj) adjp = "adj."    # adj for adjusted R2
  l <- list(a = format(coef(m)[1], digits=digits),
            b = format(abs(coef(m)[2]), digits=digits),
            pv = pv,
            r2 = format(summary(m)[[paste0(adjp,"r.squared")]], digits=digits));
  if (coef(m)[2] >= 0){
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~ italic(r)^2==r2*","~ italic(p)==pv,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~ italic(r)^2==r2*","~ italic(p)==pv,l)
    #     eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2~","~~italic(p)~"="~pv,l)
  }
  #   return(as.character(as.expression(eq)) )
  return(eq )
}
# p = addAnnotation(p, x=60, y=95, geom="text", label=lm_eqn(m, digits=2), color="black", size=12, fontface=3, angle=0, hjust=0, vjust=0)

#' lmp
# lmp(lfit13)
#' @export
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}


#' ppggs
#' 
#' @export
ppggs <- function(dfg, geom = "pointline", ...){pggs(dfg, geom = geom , ...)}
{
  ppggs <- pggs
  ff <- formals(ppggs)
  ff$geom = "pointline"
  formals(ppggs) <- ff
  rm(ff)
}

#' fffpggs
#' 
#' @export
fffpggs <- function(dfg, geom = "pointline", ...){pggs(dfg, geom = geom , ...)}
{
  fffpggs <- pggs
  ff <- formals(fffpggs)
  ff$facet_w = "nothing"
  ff$foi = "processName"
  formals(fffpggs) <- ff
  rm(ff)
}


# old:
# ppggs <- function(dfg, geom = "pointline", ...){
#   pggs(dfg, geom = geom , ...)
# }
