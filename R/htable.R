#' htable
#' @export
htable <- function(dtab
                   , lhs = aphVariables(dtab)
                   , rhs = aphFactors(dtab)
                   , long = TRUE
                   , doplot = TRUE
                   , makeBlank = c("0")[0]
                   , width = 2
                   , fun.aggregate = hmean
                   , value.var = "N"
                   , fill = 0
                   , sep = "_|_"
                   , dolog = FALSE
                   , putNAtoZero = FALSE
                   , ...
){
  dtab <- copy(dtab)
  if (is.character(fun.aggregate)){
    fun.aggregate <- match.fun(fun.aggregate)
  }
  lhs <- intersect(names(dtab), lhs)
  rhs <- intersect(names(dtab), rhs)
  if (length(lhs) == 0){
    lhs <- rhs[1]
  }
  rhs <- setdiff(rhs, lhs)
  
  log_info("lhs = {lhs}")
  log_info("rhs = {rhs}")
  stopifnot(!any(isTRUE(is.na(lhs)), isTRUE(is.na(rhs))))
  if (width > 0){
    intFactorsToReformat <- c("wk", "weekno", "mon", "yday", "doy", "mday", "DAP", "WAP")
    if (any(intFactorsToReformat %in% lhs)){
      dtab[, (lhs) := lapply(.SD, format, width = width), .SDcols = lhs]
    }
    if (any(intFactorsToReformat %in% rhs)){
      dtab[, (rhs) := lapply(.SD, format, width = width), .SDcols = rhs]
    }
  }
  if (value.var != "N"){
    tt <- dtab
  } else {
    tt <- dtab[, .N, by = c(lhs, rhs)]
  }
  {
    if (nrow(tt) == 0){
      log_trace("empty data.table")
      return(data.table())
    }
    
    if (dolog){
      tt[, N := log10(1+N)]
    }
    if (long){
      if (doplot){
        log_debug("heatmapping {rhs[1]}, {lhs[1]}")
        if (length(rhs) > 1)       {tt <- haddKey(tt, rhs,        keyID="rhsKey", sep = sep)     ; rhs = "rhsKey"}
        if (length(lhs) > 1)       {tt <- haddKey(tt, lhs,        keyID="lhsKey", sep = sep)     ; lhs = "lhsKey"}
        # tt <- copy(.tt)
        if (putNAtoZero){
          tt <- hdcast(tt)
          tt <- aphMelt(tt, value.name = "N")
          tt[is.na(N), N := 0]
        }
        p <- aphHeatmap(tt
                        , xoi = rhs
                        , yoi = lhs
                        , zoi = value.var
                        , ...
        )
        .tt <<- tt
        return(p)
      }
      return(tt[])
    }
  }
  .tt <<- tt
  # cast to wide
  tt <- dcast(tt
              , makeFormula(lhs, rhs)
              , fill = fill
              , fun.aggregate = fun.aggregate
              , value.var = value.var
  )
  if (length(makeBlank)){
    tt <- as.data.table(lapply(tt, as.character))
    tt[tt == makeBlank] <- ""
  }
  
  return(tt[])
}
