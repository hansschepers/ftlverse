#' getCropPars
#' @examples \dontrun{
#'   df <- cbind(large = getCropPars(), cherry = getCropPars("Cherry"))
#'   as.data.table(df, keep.rownames = TRUE)
#' }
# @export
getCropPars <- function(segment = "large"
                            , variety = "merlice"          # ignored
                            , cycle = c("unlit", "lit")[1] # ignored
){
  switch(tolower(segment)
         , cherry = list(
           # coloring dependence on Temperature
             maturityDegreeDays = 700
           , baseTemp = 8
           # logistic growth / swelling parameters
           , initialFruitsize = 2
           , fw_max = 15
           , rel_swelling.rate_max = 0.1
           # swelling dependence on Temperature
           , swelling_temp_ref = 20
           , swellingTempSensitivity = 0.005
           # truss
           , temp.min.truss = 12
           , truss.accell = .1
           # source -sink
           , afw4sink = 15
           , LUEref = 0.008
           , CO2Requirement = 700
           , Brix = 0.08
         )
         # default (large)
         , list(
           maturityDegreeDays = 800
           , baseTemp = 8
           # logistic growth / swelling parameters
           , initialFruitsize = 2
           , fw_max = 180
           , rel_swelling.rate_max = 0.1
           # swelling dependence on Temperature
           , swelling_temp_ref = 20
           , swellingTempSensitivity = 0.005
           # truss
           , temp.min.truss = 12
           , truss.accell = .1
           # source -sink
           , afw4sink = 150
         , LUEref = 0.008
         , CO2Requirement = 700
         , Brix = 0.04
         )
  )
}
