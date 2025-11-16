# aphExtentions = c("\\.subModel$","\\.predPooled$", 
#                   "\\.actual$","\\.ml$", 
#                   "\\.pred$","\\.clean$",
#                   "\\.used$","\\.filled$",
#                   "\\.mix$","\\.check$", 
#                   "\\.shifted$", "\\.lzman$")

#' pasteRegex
#' 
#' @export
pasteRegex <- function(aphExtentions = aphConstants()$aphExtentions
                       ){
  paste0("(", paste0(aphExtentions, collapse = ")|("),paste0(")"))
}




#' baseProcessNames
#' @examples \dontrun{
#'   baseProcessNames(x = unique(.dtwEnrich$processName))
#' }
#' @export
baseProcessNames <- function(x
                             , n = 1
                             , aphExtentions = aphConstants()$aphExtentions
                             ){
  regex <- pasteRegex(aphExtentions)
  for (ii in seq(rep)){
    x <- sub(regex, "", x)
  }
  x
}




#' addProcessNameStatus
#' 
#' @export
addProcessNameStatus <- function(DT  # long only
                                 , simplifyProcessName = FALSE
                                 ){
  dtmp <- copy(DT)
  dtmp[, status := "Original"]
  
  dtmp[grepl("\\.actual$", processName), status := "Actual"]
  dtmp[grepl("\\.subModel$", processName), status := "sub_Model"]
  dtmp[grepl("\\.predPooled$", processName), status := "Predicted_Pooled"]
  dtmp[grepl("\\.pred$", processName), status := "Predicted"]
  dtmp[grepl("\\.clean$", processName), status := "Cleaned"]
  dtmp[grepl("\\.used$", processName), status := "Used"]
  dtmp[grepl("\\.filled$", processName), status := "Filled"]
  dtmp[grepl("\\.mix$", processName), status := "Mixed"]
  dtmp[grepl("\\.check$", processName), status := "Check"]
  dtmp[grepl("\\.shifted$", processName), status := "Shifted"]
  dtmp[grepl("\\.lzman$", processName), status := "LazyMan"]
  if (log_threshold() >= 600){
    log_info("status distribution")
    print(dtmp[status %in% c("Actual", "Original")
               , .N, by = .(status, processName)])
  }
  
  if (simplifyProcessName){
    regex <- pasteRegex()
    dtmp[, processName := sub(regex, "", processName)]
  }
  dtmp[]
}
