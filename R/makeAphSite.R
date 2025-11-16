#' makeAphSite
#' 
#' @export
makeAphSite <- function(ffcsv ="aphSite.csv"
                        , wd
                        ){
  wd = getCacheDir()
  todo <- read.csv(file.path(wd, ffcsv)
                   , header = FALSE
                   , stringsAsFactors = FALSE)
  todo <- todo[, 1]
  str(todo)
  ii <- 1
  for (ii in seq_along(todo)){
    line <- todo[ii]
    
    if (grepl("^TABSET", line)){
      
    }
    
  }
}
