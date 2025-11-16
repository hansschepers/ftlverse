#' aphValues
#' @examples \dontrun{
#'   # what you put in is what you get out
#'   all.equal(aphValues(), eval(formals(aphValues)$yois))
#' }
#' @export
aphValues <- function(DT = NULL
                      , yois = c("value", "i.value"
                                 , "dist", "N"
                                 , "hmean", "Average"
                                 , ".estimate", "y", "Var2")
){
  if (!is.null(DT)) yois <- intersect(yois, names(DT))
  if (length(yois) > 1){
    log_trace("aphValues|: more than one 'value' found!")
    yois <- yois[1]
  }
  if (!length(yois)){
    # add call stack in this message!?
    log_trace("aphValues|: no 'value' found!")
    if (log_threshold() < 100) stop()
  }
  yois
}

