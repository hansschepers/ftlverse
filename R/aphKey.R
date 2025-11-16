#' aphKeyOLD
#'
#' @importFrom logger log_warn
# @export
aphKeyOLD <- function(
    DT
    , dois = aphTimes(DT
                      , alwaysAtStart = alwaysAtStart
                      , ignore = ignoreAsDois
                      , caseSensitive = caseSensitive)
    , newKeys = unique(setdiff(c(aphFactors(DT), dois), c("value", "part_day", "patt", factorIgnore)))
    , alwaysAtStart = character(0)
    , ignoreAsDois = union(c("wk", "week", "weekno", "wss", "weeknoFactor")
                           , c("doy", "hrslot", "mday")[rep(
                             any(c("Time", "time", "DAP", "dateTime", "local_time") %in% names(DT)), 3)])
    , factorIgnore = ""
    , caseSensitive = TRUE
    , verbosity = 0
){
  if (is.null(DT)) return(character())
  
  setDT(DT)
  # log_trace("setting keys to {paste(newKeys, collapse = '|')}")
  if (length(newKeys) > 0){
    setkeyv(DT, newKeys)
  }
  # print(key(DT))
  nonKeys <- setdiff(names(DT), c(newKeys, factorIgnore))
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
  newKeys
}



#' aphKeypOLD
#'
#' ProcessNames before factors!
#'
#' @importFrom logger log_warn
# @export
aphKeypOLD <- function(DT
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
