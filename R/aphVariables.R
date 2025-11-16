#' aphVariables
#'
#' @export
aphVariables <- function(DT = NULL
                         , fois = c("panel", "variable", "processName", "observation.name")
                         , verbose = FALSE) {
  if (!is.null(DT)) {
    fois <- intersect(fois, names(DT))
    if (length(fois) > 1){
      if (verbose) log_warn("aphVariable|: more than one variable / processName / observation.name found!")
      fois <- fois[1]
    }
    if (!length(fois)){
      # add call stack in this message!?
      if (verbose) log_warn("aphVariable|: no variable / processName / observation.name found!")
      if (log_threshold() < 100) stop()
    }
  }
  # if (!length(fois)) fois <- "none"
  return(fois)
}
