#' aphKey
#'
#' @importFrom logger log_warn
#' @export
aphKey <- function(
    DT
    , dois = "auto"
    , newKeys = "auto"
    , ignoreAsDois = "auto"
    , ignoreAsFois = character()
    , alwaysAtStart = character(0)
    , factorIgnore = ""
    , caseSensitive = TRUE
    , verbosity = 0
){
  if (is.null(DT)) return(character())
  DT <- copy(as.data.table(DT))
  keep <- names(DT)
  DT <- DT[, ..keep]
  alwaysIgnoreAsFois <- grep("status_", names(DT), value = TRUE)
  ignoreAsFois <- union(ignoreAsFois, c(alwaysIgnoreAsFois, factorIgnore))
  
  if (ignoreAsDois[1] == "auto"){
    ignoreAsDois <- character()
    if (any(c("Time", "time", "DAP", "dateTime", "local_time") %in% names(DT))){
      ignoreAsDois <- union(ignoreAsDois
                            , c("wk", "week", "weekno", "wss", "WAP", "weeknoFactor"
                              , "mon", "month", "hr", "hour", "tod"
                              , "doy", "hrslot", "mday"))
    }
    # str(ignoreAsDois)
  }
  
  if (dois[1] == "auto"){
    dois <- aphTimes(DT, caseSensitive = caseSensitive)
    dois <- setdiff(dois, ignoreAsDois)
    # str(dois)
  }
  
  if (newKeys[1] == "auto"){
    newKeys <- unique(setdiff(c(aphFactors(DT), dois)
                              , c("value", "part_day", "patt", ignoreAsFois)))
  }
  newKeys <- union(alwaysAtStart, newKeys)
  
  setDT(DT)
  # log_trace("setting keys to {paste(newKeys, collapse = '|')}")
  if (length(newKeys) > 0){
    setkeyv(DT, newKeys)
  }
  # print(key(DT))
  nonKeys <- setdiff(names(DT), c(newKeys, ignoreAsFois))
  if (verbosity > 600){
    if (length(nonKeys)) {
      log_warn("non-Key columns: {paste(nonKeys, collapse = ', ')}")
    }
  }
  if (length(newKeys) > 0){
    log_trace("setting keys to {paste(newKeys, collapse = '|')}")
    setkeyv(DT, newKeys)
  }
  # print(key(DT))
  invisible(structure(DT, newKeys = newKeys))
}



#' aphKeyp
#'
#' ProcessNames before factors!
#'
#' @importFrom logger log_warn
#' @export
aphKeyp <- function(DT
                    , dois = aphTimes(DT)
                    , newKeys = unique(c(aphFactors(DT), dois))
                    , verboseCheck = FALSE
){
  setDT(DT)
  nonKeys <- setdiff(names(DT), c(newKeys, "value"))
  if (verboseCheck){
    if (length(nonKeys)) {
      log_warn("non-Key columns: {paste(nonKeys, collapse = ', ')}")
    }
  }
  if (length(newKeys) > 0){
    setkeyv(DT, newKeys)
  }
  newKeys
}
