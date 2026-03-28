#' rmDotObjects
#' remove objects whose name starts with "." in .GlobalEnv (with custom exceptions)
#' @param keep [character] vector with names of objects to keep
#' @export
rmDotObjects <- function(keep = character()
                         , verbosity = log_threshold()){
  keep <- union(keep, c(".Random.seed"
                        , ".preStartSearchList"
                        , ".claude_home", ".logging_env"
                        , ".rs.WorkingDataEnv"
                        , ".__global__"
                        , ".onAttach"
                        , ".onDetach"       
                        , ".onUnload"
                        , ".sourcedRFiles"
                        , paste0(".ls", 0:9)
  ))
  if (verbosity > 500){
    message("rmDotObjects| sys.call")
    print(sys.call())
  }
  
  # dotObjects <- ls(all.names = TRUE, envir = .rs.CachedDataEnv)
  dotObjects <- ls(all.names = TRUE, envir = .GlobalEnv)
  dotObjects <- grep("^\\.", dotObjects, value = TRUE)
  dotObjects
  dotObjects <- setdiff(dotObjects, keep)
  if (verbosity > 500){
    message("rmDotObjects | cleaning dotObjects :") ; print(dotObjects)
  }
  rm(list = dotObjects, envir = .GlobalEnv)
  dotObjects
}

#' @export
removeDotObjects <- rmDotObjects
