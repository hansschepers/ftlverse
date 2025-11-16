#' tent
#' stylized stimulus function (e.g. for radiation during a day)
#' @examples \dontrun{
#' tent(Time = 0.55)
#' tent(seq(0, 1, .05), 0, 0.5, 24)/20
#' Time <- seq(0, 3, 3/160)
#' plot(tent(Time), type = "l")
#' in default, unit of Time is DAYS, the parameters are in HOURS!
#' }
#' 
#' @param Time double, time in days 
#' @param daystart double, hours after midnight when sun rises
#' @param peak double, [0, 1] if positive, time of maximum during daylength
#' @param daylength double, in hours
#' @export
tent <- function(Time
                 , daystart = 6
                 , peak = 0.5
                 , daylength = 12
                 , shape = c("flat", "block", "tent")[3]
                 , daySum = 1
                 , conserveDaySum = TRUE
                 , perio = 24
){
  stimstart <- daystart
  stimend   <- stimstart + daylength
  stimpeak  <- stimstart + daylength*max(0.001, peak)
  
  tt <- (Time%%1)*perio
  st <- 1
  if( shape == "flat"){ 
    st <- 1 
  } 
  if( shape == "block"){ 
    st <- perio / daylength * (( tt > stimstart ) & (tt < stimend))
  }
  
  if (shape %in% c("triangle", "tent")){ 
    slope1 <-  (2/daylength) / ((stimpeak - stimstart)/perio)
    slope2 <- (-2/daylength) / ((stimend  - stimpeak) /perio)
    st <- pmin( pmax(0, slope1 * (tt - stimstart) )
                , pmax(0, slope2 * (tt - stimend ) ))
  } 
  return(daySum * st)
}
