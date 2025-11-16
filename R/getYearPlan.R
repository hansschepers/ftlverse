#' getYearPlan
#' 
#' TO FIX!! sirad::ap is not good...
#' 
#' @examples \dontrun{
#'   getYearPlan(lat = 51, startYear = 2021, nWeeks = 5, startWeek = 50)
#' }
#' 
#' @export
getYearPlan <- function(lat = 51
                        , startYear = 2022
                        , nWeeks = 50
                        , startWeek = 1
                        , plot_syn = "test"
){
  dtw <- data.table(dateTime = ISOdatetime(startYear, 1, 1, 12, 0, 0) + 
                      lubridate::weeks(startWeek + seq(0, nWeeks-1)) )
  dtw[, plot_syn := plot_syn]
  dtw[, light.sum.total.day := getGlobalRad(dateTime
                                            , lat = lat
                                            , sunshine = 9) * 100]
  dtw[]
}
