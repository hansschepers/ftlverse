# added by autoDocument, row.2185 | Tue Mar 28 16:32:47 2017
#' clean2
#'
#' @author Hans.Schepers@@gmail.com
#' @export
clean2 <- function(m, co=seq(1:ncol(m))) {
  m = as.data.frame(m)
  m.c = m
  nms = names(m)
  names(nms) = nms
  for (i in co){
    if (nms[i] %in% names(m)){
      m = m[!( is.na(m[,i]) | is.infinite(m[,i]) ),]
    }
  }
  if (prod(dim(m)) == 0) {
    m = m.c
  }
  return(m)
}


# added by autoDocument, row.2159 | Tue Mar 28 16:32:47 2017
#' cleanSD0
#'
#' @author Hans.Schepers@@gmail.com
#' @export
cleanSD0 <- function(m, co = seq(1:ncol(m))
                     , except = character()) {
  wasDT <- inherits(m, "data.table")
  m <- as.data.frame(m)
  if (is.numeric(co))   co = intersect(co, seq(length(names(m))))
  if (is.character(co)) co = intersect(co,            names(m))
  m.c = m
  nam = setNames(rep(TRUE, ncol(m)), names(m))
  for (i in co) {
    if (is.numeric(m[,i]) ) {
      nam[i] = suppressWarnings ( diff(range(m[,i], na.rm=T)) > 1e-6 )
    }
    if(sum(!is.na(m[, i])) == 0) nam[i] <- FALSE
    if (names(m)[i] %in% except) nam[i] <- TRUE
  }
  m = m[, unname(nam), drop=FALSE]
  if (prod(dim(m)) == 0) m = m.c
  if (wasDT) setDT(m)
  return(m)
}






#' getyois0
#'
#' @author Hans.Schepers@@gmail.com
#' @export
getyois0 <- function(dfg) {
  dfg <- as.data.frame(dfg)
  names(dfg)[sapply(dfg, is.numeric)] # class) %in% c("numeric", "integer"
}

#' getfois0
#' 
#' @export
getfois0 <- function(dfg) {
  dfg=as.data.frame(dfg)
  names(dfg)[
    unlist(
      lapply(dfg, 
             function(x) {
               ("factor" %in% unlist(class(x))) | ("character" %in% unlist(class(x)))
             }
      )
    )
    ]
}



# added by autoDocument, row.615 | Tue Mar 28 16:32:47 2017
#' getyois
#'
#' @author Hans.Schepers@@gmail.com
#' @export
getyois <- function(dfg){
  # dfg = as.data.frame(dfg) # in case it enters as data table
  yois = getyois0(dfg)
  #   names(yois) = yois
  # yy = NULL
  yois = yois[!tolower(yois) %in%
                c("datetime", "woy", "weeknum", "week", "doy", "hour", "doyhr", "minute", "timeslot", "nr", "hsnr", "orignr", "index",
                  "noy", "moy", "night", "station", "doyhr", "DAP", "DAS", "WAP", "WAS",
                  "data.delphy", "sample.count", "sample.wt.grams",
                  "plantid", "dummy", "year", "week.plot", "week.plot.orig", "ok2use", "autumn.fall",
                  "phtime", "phtime.orig", "foikey", "groupkey", "keyA", "time", "monthday", "weekday", "month2", "igtime", "wgtime", "wdss", "data.sequence.number", "data.flags",
                  "device.id", "grower.id", "afdeling", "use", "compartment", "slkey.match", "entry", "entry.nr",
                  "test.set.entry.number", "rep.number", "plot.number", "barcode", "comments",
                  "group.1", "absolute.range", "absolute.column", "id",
                  "observation.count", "observation.rating.count", "longitude", "latitude",
                  "plot.length", "plot.width", "harvest.plot.length", "harvest.plot.width", "yearobs",
                  "exit", "ok.to.chart", "season.start.length", "organic.0.1", "lit.0.1", "has.physics", "month.nr", "fruit.weight.rank.integer", #"keep",
                  "year.decimal", "month.as.text", "groep.jr.mnd", "year.month", "fruitid", "complete.record", "season", "batch",
                  "common.with.nafta", "mabc", "post.harvest.age", "bench.ok", "r.d.other","td.fab", "repeats", "synonymhs", "pmc.short", "pink",
                  "loose.truss", "truss.position", "dsa.code", "dsa.code.2", "countna", "sample.within.variety", "heating", "sensory.session", "sensory.position",
                  "session", "product", "repetition", "postharvest.age", "stemid", "jaar", "wkss", "woy.plot", "wk2014", "wk2015", "wk2013",
                  "rij", "row", "rownr", "mat", "plant", "weeknum.mi", "weeknum.ma", "rep", "varietynr", "organic", "lit",
                  "plantid.in.variety", "westward", "vcode", "idcode", "phtime.m", "phtime.d", "pos.top", "pos.bot", #"fruits.per.truss",
                  "fruit", "position.xls", "incubation.tray", "position", "weight.q", "position.q", "gutter",
                  "Genre.N", "date.of.harvest", "date.of.arrival", "date.of.measurement", "lag", "autumn", "setnr",
                  "max.stem.density", "density", "max.stems.rootstock", "belicht", "plant.density", "judges", "would.buy.raw",
                  "weekno", "field", "side", "field.nr", "stems.pce", "m2", "ok", "fruit.id", "customer.id", "vcode",
                  "dss", "dbe", "wss", "wbe", "dayslot", "tr.slot", "gtime", "weekslot", "weektime", "ok.to.use", "slkey-match", "lit.code",
                  paste("PC", seq(20), sep=""), "measurement.1", "measurement.2", "measurement.3", "month", "truss") ]
  # for (i in yois) yy = c(yy, is.numeric(dfg[,i]) )
  # yois = yois[yy]
  return(yois)
}
yoisNum = getyois


# added by autoDocument, row.650 | Tue Mar 28 16:32:47 2017
#' getfois
#'
#' @author Hans.Schepers@@gmail.com
#' @export
getfois <- function(dfg, strict=TRUE, makedummy=T){
  if (class(dfg)[1] == "character") dfg = get(dfg)
  dfg = as.data.frame(dfg) # in case it enters as data table
  yois = getyois(dfg)
  fois = names(dfg)[!names(dfg) %in% yois]
  yy = NULL ; for (i in fois) yy = c(yy, lubridate::is.Date(dfg[,i]) )  #POSIXt
  if (!is.null(yy)) fois = fois[!yy]
  
  if(strict) fois = fois[!fois %in% c("nr", "vcode", "label", "time", "hsnr", "orignr",
                                      "data.sequence.number", "data.flags", "ok2use",
                                      "date", "date.of.arrival", "date.of.measurement", "date.of.harvest",
                                      paste("PC", seq(20), sep="") ) ]
  if(!length(fois)) fois = NULL
  if(!length(fois)) {
    fois = "dummy"
    dfg$dummy = "d"
    attr(fois, "dfg") = dfg
    message("unpack dfg from attribute 'dfg', which contains new column 'dummy' (which is hten the only factor)")
  }
  return(fois)
}
