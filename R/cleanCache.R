#' cleanCache
#' 
#' @examples \dontrun{
#'   logger::log_threshold(DEBUG)
#'   cleanCache()
#' }
#' 
#' @export
cleanCache <- function(cacheDir
                           , patterns = c(".Rmd", ".rmd", ".png")
                       ){
  if (missing(cacheDir)){
    cacheDir <- file.path(getCacheDir()
                          , Sys.getenv("USERNAME")
                          ,"/vrmdCache")
  }
  if (!dir.exists(cacheDir)) return("dir not found")
  ee <- patterns[1]
  for (ee in patterns){
    torm <- list.files(path = cacheDir, pattern = ee, full.names = TRUE)
    # str(torm)
    # print(torm)
    # log_debug("removing: {torm}")
    file.remove(torm)
  }
}
