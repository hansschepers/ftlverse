#' aphSmooth
#' 
#' @export
aphSmooth <- function(dfg
                      , yois2smooth = c("hmin", "hmax", "hmean", "value")
                      , fois2smoothby = "processName"
                      , suffix = ""
                      , n = 3
                      , reps = 1
                      , align = "center"
                      , padType = c("tailValue", "mirror", "mean"
                                    , "copy", "circular"
                                    , "constant", "NA")[1]
                      # , ...
                      ){
  dd <- as.data.table(dfg)
  yois2smooth <- intersect(yois2smooth, names(dfg))
  fois2smoothby <- intersect(fois2smoothby, names(dfg))
  for (ysmoi in yois2smooth){
    bak <- dd[,.(base::get(ysmoi))]
    log_trace("smoothing ",ysmoi)
    # message("21")
    # message(fois2smoothby)
    # dd[,{print(dim(.SD))}, by = c(fois2smoothby)]
    # message("23")
    ysmoiOUT <- paste0(ysmoi, suffix)
    dd[!is.na(get(ysmoi))
       , (ysmoiOUT) := list(hfrollmean(.SD[, base::get(ysmoi)]
                                    , n = n
                                    , reps = reps
                                    , align = align
                                    , padType = padType
                                     )
    ), by = c(fois2smoothby)]
    
    if (is.na(sum(unlist(dd[,.(range(base::get(ysmoi), na.rm=TRUE))])))){
      log_trace("Smoothing failed, reverting..")
      dd[, c(ysmoi) := list(unlist(bak)) ]
    }
  }
  dd
}