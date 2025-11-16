#' getSteeringPars
#' @examples \dontrun{
#'   df <- cbind(large = getSteeringPars(), cherry = getSteeringPars("Cherry"))
#'   as.data.table(df, keep.rownames = TRUE)
#' }
#' @export
getSteeringPars <- function(segment = "large"
                            , variety = "merlice"          # ignored
                            , cycle = c("unlit", "lit")[1] # ignored
){
  switch(tolower(segment)
         , cherry = list(temp.night = 16
                         , RTR.c = 0.004
                         , co2.level.day = 700
                         , headRemovalWeek = 40
         )
         # default (large)
         , list(temp.night = 16
                , RTR.c = 0.003
                , co2.level.day = 700
                , headRemovalWeek = 40
         )
  )
}
