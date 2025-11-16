#' radCos
#' @examples \dontrun{
#'   radCos(seq(0, 350, 50))
#'   ppggs(radCos(seq(0, 350, 7), 500, 2000, 10, 35))
#' }
#' @export
radCos <- function(Time
                   , SeasonalRadiationMinimum = 250
                   , SeasonalRadiationMaximum = 2750
                   , SeasonalRadiationShift = 10
                   , plantingCalenderWeek = 0
                   , daysPerWeek = 7
                   , daysPerYear = 365){
  rad.dri <- SeasonalRadiationMinimum +
    (SeasonalRadiationMaximum - SeasonalRadiationMinimum) *
    (1 - cos(( Time + SeasonalRadiationShift + plantingCalenderWeek * daysPerWeek ) /
               daysPerYear * 2 * 3.1415)) / 2
  # .rad.dri <<- rad.dri
  rad.dri
}
# if ("latitude" %in% names(Pars)){
# elevation <- ifelse(is.null(Pars$elevation), 0, Pars$elevation)
# drivers$radiation.driver <- solrad::OpenRadiation(
#   DOY = 0.5 + Time# + plantingCalenderWeek * daysPerWeek # yday(plantingDate)
#   , Lat = latitude, Elevation = elevation
#   , Lon=10, SLon=10, DS=0)
# } else {
