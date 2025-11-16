#' detectEvent
#' 
#' @export
detectEvent <- function(DT
                        , filter = quote(setting.fruits.m2.cu > 0)
                        , rows = TRUE
                        , FUN = c("nothing", "first", "last")[1]
                        , select = "local_time"
                        , bycols = "cropseason_id"){
  nothing <- function(x, ...) x
  DT[eval(filter)
     , setNames(list(dummy =  do.call(FUN, list(x = get(select)))), select)
     , by = bycols][rows]
}

# yield_threshold <- .5
# setting_starting_threshold <- 0
# ddw
# ddw[, .N, by = cropseason_id]
# firstSettingDate <- detectEvent(ddw
#                                 # , quote(setting.fruits.m2.cu > setting_starting_threshold)
#                                 , quote(yield.cu > yield_threshold)
#                                 , FUN = 'first'
#                                 , rows = 1
#                                 , select = "local_time"
#                                 , bycols = "cropseason_id")
# firstSettingDate
# 
# if(!exists("fitFileParms")) fitFileParms <- list()
# fitFileParms$plantingFirstFloweringDelay <- as.numeric(difftime(
#   firstSettingDate$local_time, plantingDate
#   , unit = "days"))
