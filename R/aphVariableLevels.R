#' isLong
#' @export
isLong <- function(dt){
  any(c("panel", "variable"#, "kpi"  ??
        , "processName", "observation.name") %in% names(dt))
}


#' isWide
#' @export
isWide <- function(dt){
  !isLong(dt)
}


#' aphVariableLevels
#' similar objective as getyois / getyois0 had
#' 
#' @export
aphVariableLevels <- function(dt
                              , direction = c("auto", "wide", "long")[1]
                              , subtractTimes = TRUE
){
  if (is.null(dt)){
    log_fatal("aphVariableLevels| argument 'dt' is NULL !!!!!!!!!!!!!!!!!!!!!!")
    return(character())
  }
  dt <- as.data.table(dt)
  if (direction == "auto"){
    direction <- ifelse(isLong(dt), "long", "wide")
  }
  if (direction == "wide"){
    yois <- sapply(dt, inherits, "integer") | sapply(dt, inherits, "numeric")
    yois <- names(yois)[yois]
  }
  if (direction == "long"){
    variable.name <- aphVariables(dt)[1]
    yois <- unlist(dt[, ..variable.name])
  }
  yois <- setdiff(yois, aphFactors(dt))
  if (subtractTimes) {
    yois <- setdiff(yois, aphTimes(dt))
  }
  
  # yois <- as.character(yois)
  yois <- unique(yois)
  return(yois)
}
