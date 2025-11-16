#' periodicStimulus
#' stylized stimulus function (e.g. for stylized radiation during a day)
#' 
#' unit of Time is DAYS, the parameters are in HOURS!
#' @examples \dontrun{
#'   x <- periodicStimulus(Time = seq(0, 2, .1), daystart = 6, peak = 0.5, daylength = 12, daySum = 1)
#'   mean(x)
#'   plot(x)
#'   periodicStimulus(Time = seq(0, 2, .1), daystart = 6, peak = -2, daylength = 12, daySum = 1)
#'   periodicStimulus(Time = seq(0, 2, .1), daystart = 6, peak = -1, daylength = 12, daySum = 1)
#'   x <- periodicStimulus(Time = (1:100)/10, daystart = 0, peak = -1
#'                   , daylength = 24*(3/(3+1)), daySum = 3)
#'   mean(x)
#'   plot(x)
#' } 
#' @param Time double, time in days 
#' @param daystart double, hours after midnight when sun rises
#' @param peak double, [0, 1] if positive, time of maximum during daylength
#' @param daylength double, in hours
#' @export
periodicStimulus <- function(Time
                             , daystart = 6
                             , peak = 0.5
                             , daylength = 12
                             , daySum = 1
){
  stimstart = daystart
  stimend   = stimstart + daylength
  stimpeak  = stimstart + daylength*max(0.001, peak)
  
  stimtype = "triangle"  # tent
  if (peak == -2) stimtype = "flat"
  if (peak == -1) stimtype = "block"
  
  tt = (Time%%1)*24
  if( stimtype == "flat"){ st = rep(1, length(tt)) } 
  if( stimtype == "block"){ 
    st <- sapply(tt, \(x)
                 if (( x > stimstart ) & (x < stimend)) {
                   24 / daylength
                 } else {              
                   0 
                 })
    st
  }
  if( stimtype == "triangle"){ 
    slope1 =  (2/daylength) / ((stimpeak - stimstart)/24)
    slope2 = (-2/daylength) / ((stimend  - stimpeak) /24)
    st = pmin( pmax(0, slope1 * (tt - stimstart) )
               , pmax(0, slope2 * (tt - stimend ) ))
  } 
  daySum * st
  mean(daySum * st)
  return(daySum * st)
}

