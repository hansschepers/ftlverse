#' rel2abs
#' @param RH relative humidity
#' @param Temp temperature in degree celcius
#' @details Clausius-Clapeyron formula is used to calculate the saturated vapor
#'   pressure (at 1 bar) at the given temperature. With the relative humidity,
#'   the actual vapor pressure is calculated. Finally, the absolute humidity is
#'   returned.
#' @return the absolute humidity (g/m3)
#' @examples \dontrun{
#' Temp <- 25-10*cos(seq(0,2*pi,by=1/(2*pi)))
#' RH <- 70+20*cos(seq(0,2*pi,by=1/(2*pi)))
#' AH <- rel2abs(RH,Temp)
#' DT <- enrichDT(DT, fn = 'rel2abs',
#'       subsetexpr = quote(grepl('^raw$',processName)),
#'       RH = 'humidity', Temp = 'temperature',
#'       )
#' }
#' @export
rel2abs <- function(RH,Temp){
   absHum <- with(psychroConst(),{
      es <- satvp(Temp)
       e <- RH*es/100
       absTemp <- Temp + T0 
       absHum <- Ptot*e/(Rw * absTemp)
 })
 return(absHum)
}


#' abs2rel
#' @param Xa absoulute humidity (g/m3)
#' @param Temp temperature in degree celcius
#' @details Clausius-Clapeyron formula is used to calculate the saturated vapor
#'   pressure (at 1 bar) at the given temperature. With the absolute humidity,
#'   the actual vapor pressure is calculated. Finally, the relative humidity is
#'   returned.
#' @return the relative humidity (%)
#' @examples \dontrun{
#' Temp <- 25-10*cos(seq(0,2*pi,by=1/(2*pi)))
#' Xa <- 70+20*cos(seq(0,2*pi,by=1/(2*pi)))
#' RH <- rel2abs(Xa,Temp)
#' }
#' @export
abs2rel <- function(Xa,Temp){
  RH <- with(psychroConst(),{
    es <- satvp(Temp)
    absTemp <- Temp + T0 
    e <- Xa*Rw*absTemp/Ptot
    RH <- 100*e/es
  })
  return(RH)
}


#' rel2vpdHS
#' @param RH relative humidity
#' @param Temp Temperature in degree Celcius
#' @return vapor pressure deficiency (hPa)
#' @return vapor pressure deficiency (hPa)
#' @export
rel2vpdHS <- function(RH, Temp){
  e_sat <- 0.611 * exp(17.502 * Temp / (Temp + 240.97))
  e_act = e_sat * RH/100
  e_sat - e_act
}

#' rel2vpd
#'
#' @param RH relative humidity
#' @param Temp Temperature in degree Celcius
#' @return vapor pressure deficiency (hPa)
#' @examples \dontrun{
#' Temp <- 25-10*cos(seq(0,2*pi,by=1/(2*pi)))
#' RH <- 70+20*cos(seq(0,2*pi,by=1/(2*pi)))
#' rel2vpd(RH,Temp)
#' rel2vpdHS(RH,Temp)
#' DT <- enrichDT(DT, fn = 'rel2vpd',
#'       subsetexpr = quote(grepl('^raw$',processName)),
#'       RH = 'humidity', Temp = 'temperature',
#'       )
#' }
#' @export
rel2vpd <- function(RH,Temp){
  es <- satvp(Temp)
  e <- RH*es/100
  vpd <- es - e
  vpd <- vpd/10 # Conversion from hPa to kPa
  return(vpd)
}


#' satvp
#' @param Temp temperature in degree celcius
#'
#' @details Clausius-Clapeyron formula is used to calculate the saturated vapor
#'   pressure (at 1 bar) at the given temperature.
#'
#' @return the saturated vapor pressure given the temperature in hPa
#' @export
satvp <- function(Temp){
  es <- with(psychroConst(),{
    absTemp <- Temp + T0 
    es <- esT0*exp((L/Rw) * (1/T0 - 1/absTemp))
  })
  return(es)
}

#' psychroConst
#'
#' @details constants to be used to calculate vapor pressure
#'
#' @return saturated vapor pressure at the given temperature
#' @export
psychroConst <- function(){
  return(list(
    T0 = 273.15
    , L = 2.5e6
    , Rw = 461.52
    , esT0 = 6.11
    , Ptot = 101000))
}
