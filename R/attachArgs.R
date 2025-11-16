#' hdetach
#' @examples \dontrun{
#'   # to clean up:
#'   hdetach(grep("^APH_attachedArgs", search()))
#'   hdetach("constantsAPH")
#' }
#' @export
hdetach <- function(pos = "APH_attachedArgs"){
  if (!length(pos)) return(1L)
  if (!is.character(pos)) return(1L)
  while(pos %in% search()) detach(pos
                                  , character.only = TRUE
                                  , unload = TRUE
                                  , force = TRUE)
}



#' attachArgs
#' @examples \dontrun{
#'   # to clean up:
#'   attachArgs(detachOnly = TRUE)
#'   attachArgs(hformals(match.fun("attachArgs")))
#'   rmGlobal
#'   hdetach()
#'   rmGlobal
#' }
#' @export
attachArgs <- function(argList
                       , pos = "APH_attachedArgs"
                       , detachOnly = FALSE
                       , rmGlobal = FALSE){
  
  hdetach(pos)
  if (detachOnly) return(invisible(1L))
  
  # isnull <- unlist(lapply(argList, function(x) is.null(x)))
  # argListnull <- argList[isnull]
  
  # argList <- argList[!isnull]
  # ok <- unlist(lapply(argList, function(x) (x != "")))
  # ok <- ok & (!grepl("\\(", argList))
  # argList <- argList[ok]
  # 
  # argList <- c(argList, argListnull)
  
  attach(as.list( argList ), name = pos)
  
  if(rmGlobal) {
    to_rm <- intersect(ls(".GlobalEnv"), names(argList))
    message("removing from globalEnv:")
    print(to_rm)
    rm(list = to_rm, pos = ".GlobalEnv", inherits = FALSE)
  }
  # print(search())
  # print(ls(pos))
  argList
}
