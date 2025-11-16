#' aphMeltOLD
#' @export
aphMeltOLD <- function(DT
                  , dois = aphTimes(DT, caseSensitive = caseSensitive)
                  , fois = aphFactors(DT)
                  , newKeys = unique(c(fois, dois))
                  , id.vars = aphKeyOLD(DT, newKeys = newKeys, caseSensitive = caseSensitive)
                  , defaultVariable = c("processName", "variable")[1]
                  , variable.name = ifelse(defaultVariable %in% id.vars, "kpi", defaultVariable)
                  , variable.factor = FALSE
                  , value.name = "value"
                  , ignore = c("wk", "week", "weekno", "wss", "weeknoFactor")
                  , verbosity = log_threshold()
                  , caseSensitive = TRUE
                  , ...
){
  if (is.null(DT)) {
    log_fatal("aphMelt:| DT is null, cannot melt, returning NULL")
    return(NULL)
  }
  if (verbosity >= 600) log_debug("dois, {paste(dois, collapse = ',')}")
  if (verbosity >= 600) log_debug("id.vars, {paste(id.vars, collapse = ',')}")
  if (verbosity >= 600) log_debug("variable.name, {paste(variable.name, collapse = ',')}")
  wasDT <- inherits(DT, "data.table")
  DT <- as.data.table(DT)
  if (is.null(id.vars)) id.vars <- character(0)
  DTm <- data.table::melt(makeDouble(DT)
              , id.vars = id.vars
              , value.name = value.name
              , variable.name = variable.name
              , variable.factor = variable.factor
              , ...
              )
  if (variable.factor){
    DTm[, (variable.name) := factor(get(variable.name), levels = unique(get(variable.name)), ordered = TRUE)]
  }
  keys <- aphKeyOLD(DTm, ignore = ignore, caseSensitive = caseSensitive)
  # keys <- aphKeyOLD(DTm, newKeys = setdiff(newKeys, ignore))
  if (!wasDT){
    # optionally order rows of data.frame as keys?
    #
    DTm <- as.data.frame(DTm)
  }
  return(DTm)
}


#' hmeltOLD
#'
#' @export
hmeltOLD <- function(DT
                  , defaultVariable = c("processName", "variable")[2]
                  , ...){
  aphMeltOLD(DT, defaultVariable = defaultVariable, ...)
}


#' makeDoubleOLD
#' prepares for melting without warnings of coercion to double from integer or logical
#'
#' @examples \dontrun{
#'   DT <- copy(DSall)
#'   makeDoubleOLD(DT)
#' }
#' @export
makeDoubleOLD <- function(DTWide
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
