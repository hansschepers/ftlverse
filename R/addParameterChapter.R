#' addParameterChapter
#' 
#' @export
addParameterChapter <- function(KB_LIST, repId){
  names(KB_LIST)
  KB_LIST$sim2displayEN
  
  vrmdApp(chapter = "Parameters") #####################################################
  table25 <- KB_LIST$dcPars25
  dict <- KB_LIST$dict
  names(table25) <- unlist(
    trapro(names(table25)
           , extraDict = as.list(setNames(dict$ee, dict$en))
           , KB_LIST = KB_LIST)
  )
  names(table25) <- sapply(names(table25), \(x) wrapPaste(x, width = 14))
  table25
  
  table6 <- KB_LIST$dcPars6
  names(table6) <- unlist(
    trapro(names(table6)
           , extraDict = as.list(setNames(dict$ee, dict$en))
           , KB_LIST = KB_LIST)
  )
  names(table6) <- sapply(names(table6), \(x) wrapPaste(x, width = 14))
  table6
  
  vrmdApp("dfg", dfg = table25, chunkId = "parameters by compartment")
  vrmdApp("dfg", dfg = table6, chunkId = "parameters by segment")
}
