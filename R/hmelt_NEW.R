#' aphMelt
#' @export
aphMelt <- function(DT
                    , dois = "auto"
                    , newKeys = "auto"  # unused, left in case it is used as arg in a call somewhere.
                    , fois = aphFactors(DT, add_ids = add_ids)
                    , add_ids = TRUE
                    , id.vars = "auto"
                    , omit = character(0)
                    , extra_id.vars = character(0)
                    , defaultVariable = c("processName", "variable")[1]
                    , variable.name = "auto"
                    , variable.factor = FALSE
                    , value.name = "value"
                    , ignoreAsFois = character()
                    , ignoreAsDois = "auto"
                    # , ignoreAsDois = c("wk", "week", "weekno", "wss", "weeknoFactor")
                    , verbosity = log_threshold()
                    , caseSensitive = TRUE
                    , keep.rownames = TRUE
                    , ...
){
  if (!length(DT)) {message("aphMelt | can't melt NULL") ; return(NULL)}
  wasDT <- inherits(DT, "data.table")
  if (keep.rownames & !is.data.frame(DT)) {  # if it was a matrix?
    DT <- as.data.table(DT, keep.rownames = T)
  }
  if ("rn" %in% names(DT)){
    extra_id.vars <- union(extra_id.vars, "rn")
  }
  DT <- copy(as.data.table(DT))
  keep <- names(DT)
  keep <- setdiff(keep, omit)
  DT <- DT[, ..keep]
  # if (!wasDT) DTm <- as.data.frame(DT)
  
  if (ignoreAsDois[1] == "auto"){
    ignoreAsDois <- character()
    # if (any(c("Time", "time", "DAP", "dateTime", "local_time") %in% names(DT))){
    #   ignoreAsDois <- union(ignoreAsDois
    #                         , c("wk", "week", "weekno", "wss", "WAP", "weeknoFactor"
    #                             , "mon", "month", "hr", "hour", "tod"
    #                             , "doy", "hrslot", "mday"))
    # }
    # str(ignoreAsDois)
  }
  
  if (dois[1] == "auto"){
    dois <- aphTimes(DT, caseSensitive = caseSensitive)
    # str(dois)
  }
  
  # if (newKeys[1] == "auto"){
  #   newKeys <- unique(setdiff(c(fois, dois)
  #                             , c("value", "part_day", "patt", factorIgnore)))
  # }
  # newKeys <- 
  if (id.vars[1] == "auto"){
    id.vars <- unique(c(fois, dois))
    # id.vars <- aphKey(DT, newKeys = newKeys, caseSensitive = caseSensitive)
    id.vars <- setdiff(id.vars
                       , c("value", "part_day", "patt"
                           # , factorIgnore
                       ))
  }
  id.vars <- union(id.vars, intersect(names(DT), extra_id.vars))
  
  # print(key(DT))
  if (variable.name[1] == "auto"){
    variable.name <- ifelse(defaultVariable %in% id.vars, "kpi", defaultVariable)
  }
  
  if (is.null(DT)) {
    log_fatal("aphMelt:| DT is null, cannot melt, returning NULL")
    return(NULL)
  }
  if (verbosity >= 600) log_debug("dois, {paste(dois, collapse = ',')}")
  if (verbosity >= 600) log_debug("id.vars, {paste(id.vars, collapse = ',')}")
  if (verbosity >= 600) log_debug("variable.name, {paste(variable.name, collapse = ',')}")
  wasDT <- inherits(DT, "data.table")
  DT <- as.data.table(DT)
  if (is.null(id.vars)) id.vars <- extra_id.vars# character(0)
  
  DTm <- data.table::melt(makeDouble(DT)
                          , id.vars = id.vars
                          , value.name = value.name
                          , variable.name = variable.name
                          , variable.factor = variable.factor
                          , ...
  )
  if (variable.factor){
    DTm[, (variable.name) := factor(get(variable.name)
                                    , levels = unique(get(variable.name))
                                    , ordered = TRUE)]
  }
  keys <- aphKey(DTm
                 # , ignoreAsDois = ignoreAsDois
                 , ignoreAsFois = ignoreAsFois
                 , caseSensitive = caseSensitive)
  # keys <- aphKey(DTm, newKeys = setdiff(newKeys, ignore))
  if (!wasDT){
    # optionally order rows of data.frame as keys?
    #
    DTm <- as.data.frame(DTm)
  }
  return(DTm)
}


#' hmelt
#'
#' @export
hmelt <- function(DT
                  , defaultVariable = c("processName", "variable")[2]
                  , ...){
  aphMelt(DT, defaultVariable = defaultVariable, ...)
}


#' makeDouble
#' prepares for melting without warnings of coercion to double from integer or logical
#'
#' @examples \dontrun{
#'   DT <- copy(DSall)
#'   makeDouble(DT)
#' }
#' @export
makeDouble <- function(DTWide
                       , yois = aphVariableLevels(DTWide, direction = "wide")){
  if (is.null(yois)) return(DTwide)
  if (length(yois) < 1) return(DTwide)
  if ("" %in% yois){
    log_warn("'' in names(DTwide)!, taking out")
    yois <- setdiff(yois, "")
  }
  DTWide <- as.data.table(DTWide)
  DTWide[, (yois) := lapply(.SD, as.double), .SDcols = yois]
  DTWide[]
}
