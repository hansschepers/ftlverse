#' aphQC
#  check a log, o.a. date intervals / gaps  
# @export
aphQC <- function(DTcyc
                  , fois = getfois0(DTcyc)
                  # , api = "auto"
                  , bycols = c("plot_syn", "cycle_syn")
                  , variableName = "processName"
                  # , dateFois = NULL # "resetDay"
                  , doi = "dateTime"
                  , todo = c("valueNAs", "factorNAs", "dateGaps")
                  , timeUnit = 60
                  , addRmd = TRUE
){
  DT <- copy(DTcyc)
  
  DT <- DT[!is.na(plot_syn)]
  DT <- DT[!is.na(cycle_syn)]
  
  todo <- tolower(todo)
  if (api[1] == "auto"){
    if("plot_syn" %in% names(dt)){
      api <- "agrisensys"
      bycols <- "plot_syn"
    } else {
      api <- "lg"
      bycols <- "differentiator"
    }
  }
  
  (rawFois <- getfois0(dt))
  (foisExpected <- setdiff(rawFois, doi))
  log_info("expected as factors: {paste(foisExpected, collapse = ', ')}")
  if (is.null(fois)) fois <- foisExpected
  (allFois <- union(foisExpected, dateFois))
  
  # inspect and set keys ----
  # key(dt)
  log_info("current keys: {paste(key(dt), collapse = ', ')}")
  
  assumedKeys <- c(fois, dateFois, doi)
  setkeyv(dt, assumedKeys)
  log_info("new keys: {paste(key(dt), collapse = ', ')}")
  
  
  # prepare output as list
  qaList <- list(assumedKeys = assumedKeys)
  sectionTitle = setNames( c("assumedKeys", "NA's in values", "NA's in factors", "gaps in dateTime")
                           , c("assumedKeys", "valueNAs", "factorNAs", "dateGaps"))
  
  # track NAs in all columns ----
  print(sapply(dt, sumna))
  
  # track NAs in values ----
  if ("valuenas" %in% todo) {
    valueNAs <- dt[, hsummaryC(value), by = c(foisExpected)][okData != "100"]
    qaList$valueNAs <- valueNAs
  }
  
  
  # track NAs in factors ----
  if ("factornas" %in% todo) {
    (nas <- sapply(dt[, ..allFois], sumna))
    (naFactors <- names(nas[nas > 0]))
    factorNAs <- NULL
    if (sum(nas) > 0){
      log_fatal("NAs in factors:")
      kpis <- c("hlength", "sumna", "okData")
      
      if (is.null(dateFois[1])){
        ss <- rbindlist(lapply(naFactors,
                               function(foi) {
                                 rr <- dt[, hsummary(get(foi), hfuns = kpis), by = setdiff(allFois, foi)]
                                 rr[, (foi) := NA]
                                 rr[okData < 100]
                               }), fill=TRUE    )
        factorNAs <- ss
      } else {
        # ss[, sum(hlength), by = okData]
        foi <- dateFois[1] #"resetDay"
        ss1 <- dt[, hsummaryC(resetDay, hfuns = kpis), by = c(bycols)]
        ss1[, (foi) := 'NA']
        ss1[, (variableName) := 'all']
        ss1
        
        ss2 <- dt[, hsummaryC(resetDay, hfuns = kpis), by = c(variableName)]
        ss2[, (foi) := 'NA']
        ss2[, (bycols) := 'all']
        ss2
        
        ss12 <- dt[, hsummaryC(resetDay, hfuns = kpis), by = c(bycols, variableName)][okData != "100"]
        ss12[, (foi) := "NA"]
        ss12
        factorNAs <- rbind(ss1[okData != "100"], ss2[okData != "100"], ss12, fill=TRUE)
      }
    }
    qaList$factorNAs <- factorNAs
  }
  
  
  # track gaps in dateTime ----
  if ("dategaps" %in% todo) {
    dt[, dateInterval := diff1(as.numeric(get(doi))/timeUnit), by = fois]
    # dt[plot_syn == "bryte_20_543"
    #    & processName %in% c("stem load cell raw")]
    
    allIntervals <- dt[, .N, by = c(fois, "dateInterval")]
    allIntervals
    
    mostCommon1 <- dt[, .N, by = c("dateInterval")][N > mean(N)]$dateInterval
    mostCommon <- allIntervals[, .N, by = c("dateInterval")][N > mean(N)]$dateInterval
    mostCommon
    
    tab <- dt[, .N, by = c(fois, "dateInterval")][!dateInterval %in% mostCommon]
    tab
    
    # pggs(tab, xoi = "dateInterval", yoi = "N", geom = "point", pointAlpha = 0.3
    #      , chunkTitle = "Uncommon dateTime intervals, corrected for factors, in minutes"
    #      # , foi = rawFois[2]
    #      #, facet_w = "nothing"
    #      # , facet_w = rawFois[1], free_y = TRUE, free_X = TRUE
    # )
    
    # tab[, sum(N), by = "dateInterval"]
    tab[, prettyN := hprettyNum(N)]
    dateGaps <- dcast(tab, makeFormula(lhs=fois, rhs="dateInterval"), value.var = "prettyN")
    dateGaps[is.na(dateGaps)] <- ""
    qaList$dateGaps <- dateGaps
    dateGaps
  }
  
  
  if (addRmd & exists("myGlobalRmd")){
    names(qaList) <- paste0(api, names(qaList) )
    notToReport <- paste0(api, "assumedKeys")
    lapply(setdiff(names(qaList), notToReport)
           , function(x) {
             vv <- sub(paste0("^", api),"",x)
             sec <- setNames(list(x = paste(api, sectionTitle[vv])), make.names(x))
             vrmd("dfg"
                  , section = sec
                  , dfg = qaList[[x]]
                  , chunkId = x
             )})
  }
  
  qaList$sectionTitle <- sectionTitle
  
  return(qaList)  
}

