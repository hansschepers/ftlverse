#' enrichDTsimple
#' @examples \dontrun{
#'   enrichDTsimple(DT
#'     , expression = quote(setting.truss.diff := as.numeric(paste(setting.truss, setting.truss.fruits, sep='.')))
#'     )
#' }
#' @export
enrichDTsimple <- function(DT
                           , expression 
                           , subsetExpr = TRUE
                           , dois = "dateTime"
                           , variable = NULL
                           , variable.factor = NULL
                           , value = "value"
                           , accross = NULL
                           , bycols = character(0)
                           , fois = NULL
                           , fun.aggregate = hmean
                           , afterCastExpression = NULL
                           , melt.na.rm = FALSE
                           , byRow = FALSE
                           , verbosity = 400
                           , na.rm = TRUE
                           # , ...
){
  # dots <- list(f1 = .9)
  # dots <- list(...)
  # dots <- as.environment(dots)
  # print(ls(dots))
  DT <- copy(DT)
  oldkeys <- key(DT)
  if (!dois %in% names(DT)){
    dois <- character(0)
  }
  # .DT <<- DT
  if (is.null(variable)) variable <- intersect(c("variable", "processName"), names(DT))[1]
  if (is.null(variable.factor)) variable.factor <- is.factor(DT[, base::get(variable)])
  if (is.null(fois)) fois <- setdiff(names(DT), c(dois, accross, variable, value))
  # if (is.null(afterCastExpression)) afterCastExpression <- expression(print(dim(wideDT)))
  if (inherits(fun.aggregate, "character")) {
    fun.aggregate <- match.fun(fun.aggregate)
  }
  
  # focus 
  doSplit <- !is.logical(subsetExpr)
  if (doSplit) restDT <- DT[!eval(subsetExpr), ]
  
  longDT <- DT[eval(subsetExpr), ]
  # longDT <- wDT[, .SD, .SD = usedvars]
  # str(longDT)
  # .longDT2 <<- longDT
  wideDT <- dcast(longDT
                  , as.formula(paste("... ~", variable))
                  , value.var = value
                  , fill = NA
                  , fun.aggregate = fun.aggregate
  )
  if ("." %in% names(wideDT)){
    wideDT[, . := NULL]
  }
  # .wideDT <<- wideDT
  # str(wideDT)
  
  if (!is.null(accross)){
    wideDT[, tmp := NA]
    setnames(wideDT, "tmp", accross)
  }
  
  eval(afterCastExpression)
  
  if (!is.null(expression)) {
    if (inherits(expression, "list")){
      message("executing list of expressions")
      # expression can be a list of expressions
      for (iexpr in expression){
        # dots <- parent.frame(2)
        # print(ls(dots))
        log_trace("iexpr {as.character(iexpr)}")
        wideDT[, eval(iexpr), by = setdiff(bycols, variable)]
      }
    } else {
      wideDT[, eval(expression), by = setdiff(bycols, variable)]
    }
  }
  
  id.vars <- c(fois, dois)
  measure.vars <- setdiff(names(wideDT), id.vars)
  measClasses <- sapply(wideDT[, ..measure.vars], class)
  tt <- table(measClasses)
  if (length(tt) > 1) {
    print(tt)
    print(measClasses[!measClasses %in% c("numeric")])
  }
  if (log_threshold() > 400){
    log_debug("c(fois, variable, dois) before melt ")
    print(id.vars)
    # .wideDT2 <<- wideDT
  }
  
  aphKey(wideDT)
  newDT <- melt(wideDT
                , id.vars = id.vars
                , variable.name = variable
                , value.name = value
                , variable.factor = variable.factor
                , na.rm = melt.na.rm
  )
  if (doSplit) {
    newDT <- rbindlist(list(restDT, newDT), use.names = TRUE, fill=TRUE)
  }
  # aphKey(newDT)
  if (na.rm){
    newDT <- newDT[!is.na(value)]
  }
  if (length(oldkeys)){
    setkeyv(newDT, oldkeys)  # unique(c(fois, bycols, variable, dois))
  }
  newDT <- newDT[]
  return(newDT)
}

