#' splitApply
#' 
#' @export
splitApply <- function(dt
                       , FUN 
                       , bycols = setdiff(aphFactors(dt), "processName")
                       , useLapply = T#FALSE
                       , ...){
  dt <- copy(as.data.table(dt))
  dtList <- split(dt, by = bycols)
  
  if (useLapply){
    
    dtListDone <- lapply(dtList, FUN, ...)
    
  } else {
    
    dtListDone <- list()
    dots <- list(...)
    # print(strList(dots))
    for (ii in seq_along(dtList)){
      cat(ii)
      xx <- dtList[[ii]]
      # print(dim(xx))
      arg1 <- list(dtlong = xx)
      arg1 <- setNames(arg1, names(formals(FUN))[1])
      # message(ii)
      argList <- c(arg1, dots)
      # print(strList(argList))
      # .xx <<- argList[[1]]
      # stop()
      dtListDone[[ii]] <- do.call(FUN, argList)
      # .dtListDone <<- dtListDone
    }
  }
  # if (is.list(dtListDone)){
  #   dtListDone <- rbindlist(dtListDone)
  # }
  dtListDone
}
