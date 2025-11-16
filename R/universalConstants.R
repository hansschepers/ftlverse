#' universalConstants
#' 
#' @export
universalConstants <- function(){
  as.list(c(degrees10 = 10
            , JoulePerMJ = 1000000
            , cm2PerM2 = 10000
            , gramPerLiter = 1000
            , gramPerKg = 1000
            , J2W = 8.64          # J/cm2/day / (W/m2)
            , latentHeat20C = 2.453  # J/gr
            , daysPerWeek = 7     # day / week
            , daysPerYear = 365
            , hoursPerDay = 24
            , zero = 0
            , secondsPerHour = 3600
  ))
}

#' universalUnits
#' 
#' @export
universalUnits <- function(){
  as.list(c(degrees10 = "celcius"
            , JoulePerMJ = "J/MJ"
            , cm2PerM2 = "cm2/m2"
            , gramPerLiter = "grFW/L"
            , gramPerKg = "gr/kg"
            , J2W = "J/cm2/day/(W/m2)"
            , latentHeat20C = "J/gr"
            , daysPerWeek = "day/week"
            , daysPerYear = "day/year"
            , hoursPerDay = "hour/day"
            , zero = "1"
            , secondsPerHour = "s/hr"
  ))
}
