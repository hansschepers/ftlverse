#' getPrepPars
#' @examples \dontrun{
#'   getPrepPars()
#'   getPrepPars("Cherry")
#' }
#' @export
getPrepPars <- function(segment = "large"
                        , variety = "merlice"          # ignored
                        , cycle = c("unlit", "lit")[1] # ignored
){
  switch(tolower(segment)
         , cherry = list(basePruning = 14
                         , extraPruningWss = 25
                         , extraPruningDuration = 5
                         , newPruning = 16
                         # trussSpeed
                         , maxTrussSpeed = 2.5
                         , minTrussSpeed = .4
                         # stems
                         , extraStemWss = 15
                         , baseStemDensitySetting = 3.5
                         , newStemDensitySetting = 4.5)
         # default (large)
         , list(basePruning = 5
                , extraPruningWss = 25
                , extraPruningDuration = 5
                , newPruning = 6
                # trussSpeed
                , maxTrussSpeed = 2.5
                , minTrussSpeed = .4
                # stems
                , extraStemWss = 15
                , baseStemDensitySetting = 3
                , newStemDensitySetting = 3.5)
  )
}
