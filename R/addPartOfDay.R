#' addPartOfDay
#' 
#' @export
addPartOfDay <- function(dtw
                         , doi = "local_time"
                         , phasesOfDay = c("1 early (0-6hr)"
                                           , "2 morning (6-12hr)"
                                           , "3 afternoon (12-18hr)"
                                           , "4 late (18-24hr)")
){
  dtw[, part_day := phasesOfDay[1+floor(lubridate::hour(get(doi))/6)]]
  invisible(dtw)
}  
