#' withErrorAllowed
#' @export
withErrorAllowed <- function(
    expr,
    allowError = TRUE,
    errorResult = NULL) {
  tryCatch(expr,
           error = function(e) {
             if (allowError) {
               logger::log_warn(e$message)
               return(errorResult)
             }
             else stop(e)
           })
}
