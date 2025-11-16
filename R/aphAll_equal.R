#' aphAll_equal
#' 
#' @export
aphAll_equal <- function(pList
                       , objectName = "drivers"
                       , yois = c("all")
                       , doMelt = TRUE
                       , doCast = TRUE
                       , doplot = FALSE
                       , extraFuns = c("hMAPE", "MAE", "RMSE", "hlength")
                       , filterExpr = quote(MAE > 1e-5)
                       , ...){
  ii <- 1
  names(pList[[ii]])
  firstDT <- pList[[ii]][[objectName]]
  if (doMelt) firstDT <- aphMelt(firstDT)
  if ("processName" %in% names(firstDT)){
    if (!"all" %in% yois) firstDT <- firstDT[processName %in% yois]
  }
  if (doCast) firstDT <- hdcast(firstDT)
  yois <- aphVariableLevels(firstDT)
  # firstDT[, c(yois)]
  # sapply(firstDT[, ..yois], class)
  # firstDT
  res <- list()
  p_diff <- ggplot2::ggplot()
  nn <- length(pList)
  ii <- 2
  for (ii in seq_along(pList)[-1]){
    log_trace("aphAll_equal loop: ", ii)
    nextDT <- pList[[ii]][[objectName]]
    if (doMelt) nextDT <- aphMelt(nextDT)
    if ("processName" %in% names(nextDT)){
      if (!"all" %in% yois) nextDT <- nextDT[processName %in% yois]
    }
    if (doCast) nextDT <- hdcast(nextDT)
    
    differences <- mapply(FUN = `-`
                 , SIMPLIFY = FALSE
                 , firstDT[, ..yois]
                 , nextDT[, ..yois])
    differences <- as.data.table(differences)
    .differences <<- copy(differences)
    
    if (doplot){
      tmp <- copy(differences)
      tmp$Time <- firstDT$Time
      p_diff <- pggs(aphMelt(tmp), p_diff = p_diff, doplot = FALSE)
      if (ii == nn){
        print(p_diff)
      }
    }
    
    # str(differences)
    # differences[4]
    metric.s = c(hMAPE = hMAPE
                 , MAE = MAE
                 , RMSE = RMSE
                 , hlength = hlength)
    # check
    # sapply(extraFuns, \(x) formals(get(x))["x"], simplify = F)
    
    qq <- sapply(differences, hsummary, hfuns = extraFuns, simplify = F)
    qq <- rbindlist(qq, fill = T, idcol = "yoi")
    res[[ii]] <- qq
    # aphMelt(qq)
  }
  qq <- rbindlist(res, idcol = "sim")
  qq <- qq[eval(filterExpr)]
  return(qq)
}
